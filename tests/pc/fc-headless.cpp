/*
 * fc-headless: CEmu core without a GUI, driven over stdin/stdout, for
 * testing FamiCalc on a PC (see DEVNOTES.md, "PC testing").
 *
 * Based on CEmu's tests/autotester/headless_cli.cpp (GPLv3, like the CEmu core
 * it links), with commands added for holding keys, reading memory and
 * counting how often an address is executed (FPS measurement).
 *
 * Every command answers with one line starting with OK or ERR. Text the
 * calculator program prints to CEmu's debug port (dbg_printf) is passed
 * through as lines starting with "DBG ". CEmu's own messages go to stderr.
 */

#include <cctype>
#include <cerrno>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "autotester.h"

namespace cemucore
{
    extern "C"
    {
        #include "core/debug/debug.h"
        #include "core/mem.h"
        #include "core/schedule.h"
        #include "core/vat.h"
        #include "core/lcd.h"
        #include "core/cpu.h"

        static std::string console_line;

        /* snap: memory copied whenever execution reaches an address */
        struct snap_t {
            std::vector<std::pair<uint32_t, uint32_t>> regions;	/* address, length */
            std::vector<std::vector<uint8_t>> taken;
        };
        static std::map<uint32_t, snap_t> snaps;

        void gui_console_clear() {}
        void gui_debug_close(void) {}
        void gui_debug_open(int reason, uint32_t data) {
            if (reason == DBG_BREAKPOINT && snaps.count(data & 0xFFFFFF)) {
                snap_t& s = snaps[data & 0xFFFFFF];
                std::vector<uint8_t> copy;
                for (const auto& region : s.regions) {
                    for (uint32_t i = 0; i < region.second; i++) {
                        copy.push_back(mem_peek_byte((region.first + i) & 0xFFFFFF));
                    }
                }
                s.taken.push_back(copy);
                if (s.taken.size() > 16) {
                    s.taken.erase(s.taken.begin());
                }
                return;
            }
            std::fprintf(stderr, "[fc-headless] debugger event reason=%d data=0x%X\n", reason, data);
        }
        /* The core prints its own messages ("[CEmu] ...") and, one buffer at a
         * time, whatever the emulated program writes to the debug port. */
        void gui_console_printf(const char *format, ...) {
            char text[4200];
            va_list ap;
            va_start(ap, format);
            std::vsnprintf(text, sizeof text, format, ap);
            va_end(ap);
            if (!std::strncmp(text, "[CEmu]", 6)) {
                std::fputs(text, stderr);
                return;
            }
            for (const char *p = text; *p; p++) {
                if (*p == '\n') {
                    std::cout << "DBG " << console_line << std::endl;
                    console_line.clear();
                } else {
                    console_line += *p;
                }
            }
        }
        void gui_console_err_printf(const char *format, ...) {
            va_list ap;
            va_start(ap, format);
            std::vfprintf(stderr, format, ap);
            va_end(ap);
        }
        asic_rev_t gui_handle_reset(const boot_ver_t *, asic_rev_t loaded_rev,
                                    asic_rev_t, emu_device_t, bool *) {
            return loaded_rev;
        }
    }
}

namespace
{
bool parseUnsigned(const std::string& text, uint32_t& result, int base = 10)
{
    if (text.empty()) {
        return false;
    }
    char *end = nullptr;
    errno = 0;
    const unsigned long value = std::strtoul(text.c_str(), &end, base);
    if (errno || !end || *end || value > (std::numeric_limits<uint32_t>::max)()) {
        return false;
    }
    result = static_cast<uint32_t>(value);
    return true;
}

bool fileExists(const std::string& path)
{
    return !path.empty() && std::ifstream(path, std::ios::binary).good();
}

void put16(std::vector<uint8_t>& data, size_t offset, uint16_t value)
{
    data[offset] = static_cast<uint8_t>(value);
    data[offset + 1] = static_cast<uint8_t>(value >> 8);
}

void put32(std::vector<uint8_t>& data, size_t offset, uint32_t value)
{
    for (int i = 0; i < 4; i++) {
        data[offset + i] = static_cast<uint8_t>(value >> (8 * i));
    }
}

bool writeScreenshot(const std::string& path)
{
    const uint32_t row_size = (LCD_WIDTH * 3u + 3u) & ~3u;
    const uint32_t pixel_size = row_size * LCD_HEIGHT;
    std::vector<uint32_t> frame(LCD_SIZE);
    std::vector<uint8_t> bitmap(54u + pixel_size, 0);

    cemucore::emu_lcd_drawframe(frame.data());

    bitmap[0] = 'B';
    bitmap[1] = 'M';
    put32(bitmap, 2, static_cast<uint32_t>(bitmap.size()));
    put32(bitmap, 10, 54);
    put32(bitmap, 14, 40);
    put32(bitmap, 18, LCD_WIDTH);
    put32(bitmap, 22, LCD_HEIGHT);
    put16(bitmap, 26, 1);
    put16(bitmap, 28, 24);
    put32(bitmap, 34, pixel_size);

    for (uint32_t out_y = 0; out_y < LCD_HEIGHT; ++out_y) {
        const uint32_t in_y = LCD_HEIGHT - 1u - out_y;
        uint8_t *output = bitmap.data() + 54u + out_y * row_size;
        for (uint32_t x = 0; x < LCD_WIDTH; ++x) {
            const uint32_t pixel = frame[in_y * LCD_WIDTH + x];
            *output++ = static_cast<uint8_t>(pixel);
            *output++ = static_cast<uint8_t>(pixel >> 8);
            *output++ = static_cast<uint8_t>(pixel >> 16);
        }
    }

    std::ofstream file(path, std::ios::binary);
    file.write(reinterpret_cast<const char *>(bitmap.data()),
               static_cast<std::streamsize>(bitmap.size()));
    return file.good();
}

struct transfer_progress_t {
    bool finished = false;
    bool failed = false;
};

bool transferProgress(void *context, int value, int total)
{
    transfer_progress_t *progress = static_cast<transfer_progress_t *>(context);
    if (total <= 0) {
        progress->failed = true;
        progress->finished = true;
    } else if (value == total) {
        progress->finished = true;
    }
    return false;
}

bool sendFile(const std::string& path, int location)
{
    const char *file = path.c_str();
    transfer_progress_t progress;
    if (cemucore::emu_send_variables(&file, 1, location, transferProgress, &progress) !=
        cemucore::LINK_GOOD) {
        return false;
    }
    for (uint32_t elapsed = 0; !progress.finished && elapsed < 60000; elapsed += 10) {
        cemucore::emu_run(10);
    }
    if (!progress.finished) {
        return false;
    }
    cemucore::emu_run(100);
    return !progress.failed;
}

void respond(const std::string& message)
{
    std::cout << message << std::endl;
}

bool keyEvent(const std::string& name, bool pressed)
{
    autotester::key_coord_t coord{};
    if (!autotester::keyCoordForName(name, coord)) {
        return false;
    }
    cemucore::emu_keypad_event(coord.y, coord.x, pressed);
    return true;
}

/* Hit counters the caller added, so "counts" can list them. */
std::map<uint32_t, bool> counters;

bool runCommand(const std::string& line)
{
    std::istringstream input(line);
    std::string command;
    input >> command;

    if (command.empty() || command[0] == '#') {
        return true;
    }
    if (command == "quit" || command == "exit") {
        respond("OK bye");
        return false;
    }
    if (command == "run") {
        std::string value;
        uint32_t milliseconds;
        if (!(input >> value) || !parseUnsigned(value, milliseconds)) {
            respond("ERR usage: run <ms>");
        } else {
            cemucore::emu_run(milliseconds);
            respond("OK run " + std::to_string(milliseconds));
        }
        return true;
    }
    if (command == "key") {
        /* press, hold, release, then let the program see the release */
        std::string name, hold_text;
        uint32_t hold_ms = 80;
        input >> name;
        if ((input >> hold_text) && !parseUnsigned(hold_text, hold_ms)) {
            respond("ERR invalid hold duration");
            return true;
        }
        if (!keyEvent(name, true)) {
            respond("ERR unknown key " + name);
            return true;
        }
        cemucore::emu_run(hold_ms);
        keyEvent(name, false);
        respond("OK key " + name);
        return true;
    }
    if (command == "press" || command == "release") {
        std::string name;
        input >> name;
        if (!keyEvent(name, command == "press")) {
            respond("ERR unknown key " + name);
        } else {
            respond("OK " + command + " " + name);
        }
        return true;
    }
    if (command == "keys") {
        std::string sequence, error;
        std::getline(input >> std::ws, sequence);
        autotester::key_sequence_handlers_t handlers;
        handlers.keyEvent = [](uint8_t row, uint8_t col, bool pressed) {
            cemucore::emu_keypad_event(row, col, pressed);
        };
        handlers.delay = [](unsigned int ms) { cemucore::emu_run(ms); };
        handlers.error = [&error](const std::string& value) { error = value; };
        if (sequence.empty() || !autotester::runKeySequence(sequence, handlers)) {
            respond("ERR " + (error.empty() ? std::string("invalid key sequence") : error));
        } else {
            respond("OK keys");
        }
        return true;
    }
    if (command == "peek") {
        /* peek <hex address> <length>: memory as the CPU sees it, in hex */
        std::string addr_text, len_text;
        uint32_t addr, len;
        if (!(input >> addr_text >> len_text) || !parseUnsigned(addr_text, addr, 16) ||
            !parseUnsigned(len_text, len) || len > 0x10000) {
            respond("ERR usage: peek <hex-address> <length>");
            return true;
        }
        std::string out = "OK ";
        char hex[3];
        for (uint32_t i = 0; i < len; i++) {
            std::snprintf(hex, sizeof hex, "%02X", cemucore::mem_peek_byte((addr + i) & 0xFFFFFF));
            out += hex;
        }
        respond(out);
        return true;
    }
    if (command == "poke") {
        /* poke <hex address> <hex bytes> */
        std::string addr_text, bytes;
        uint32_t addr;
        if (!(input >> addr_text >> bytes) || !parseUnsigned(addr_text, addr, 16) ||
            bytes.size() % 2) {
            respond("ERR usage: poke <hex-address> <hex-bytes>");
            return true;
        }
        for (size_t i = 0; i < bytes.size(); i += 2) {
            uint32_t value;
            if (!parseUnsigned(bytes.substr(i, 2), value, 16)) {
                respond("ERR bad hex byte");
                return true;
            }
            cemucore::mem_poke_byte((addr + static_cast<uint32_t>(i / 2)) & 0xFFFFFF,
                                    static_cast<uint8_t>(value));
        }
        respond("OK poke");
        return true;
    }
    if (command == "count") {
        /* count <hex address>: count executions of the instruction there */
        std::string addr_text;
        uint32_t addr;
        if (!(input >> addr_text) || !parseUnsigned(addr_text, addr, 16)) {
            respond("ERR usage: count <hex-address>");
        } else if (counters.count(addr)) {
            respond("OK count (already)");
        } else if (!cemucore::debug_hit_counter_add(addr, nullptr)) {
            respond("ERR could not add counter");
        } else {
            counters[addr] = true;
            respond("OK count");
        }
        return true;
    }
    if (command == "snap") {
        /* snap <hex address> <hex memory> <length>: copy memory each time
         * the instruction at address runs (last 16 copies kept); more snaps
         * at the same address add regions to the same copies */
        std::string a, m, l;
        uint32_t addr, mem, len;
        if (!(input >> a >> m >> l) || !parseUnsigned(a, addr, 16) || !parseUnsigned(m, mem, 16) ||
            !parseUnsigned(l, len) || len > 0x10000) {
            respond("ERR usage: snap <hex-address> <hex-memory> <length>");
            return true;
        }
        cemucore::snaps[addr].regions.emplace_back(mem, len);
        cemucore::snaps[addr].taken.clear();
        cemucore::debug_watch(addr, DBG_MASK_EXEC, true);
        respond("OK snap");
        return true;
    }
    if (command == "snaps-all") {
        /* snaps-all <hex address> <path>: every kept copy, oldest first, into a file */
        std::string a, path;
        uint32_t addr;
        if (!(input >> a) || !parseUnsigned(a, addr, 16) || !cemucore::snaps.count(addr)) {
            respond("ERR usage: snaps-all <hex-address> <path> (after snap)");
            return true;
        }
        std::getline(input >> std::ws, path);
        std::ofstream file(path, std::ios::binary);
        auto& s = cemucore::snaps[addr];
        for (const auto& copy : s.taken) {
            file.write(reinterpret_cast<const char *>(copy.data()), static_cast<std::streamsize>(copy.size()));
        }
        respond("OK " + std::to_string(s.taken.size()));
        s.taken.clear();
        return true;
    }
    if (command == "snaps") {
        /* snaps <hex address>: "OK <count> <hex of the newest copy>" */
        std::string a;
        uint32_t addr;
        if (!(input >> a) || !parseUnsigned(a, addr, 16) || !cemucore::snaps.count(addr)) {
            respond("ERR usage: snaps <hex-address> (after snap)");
            return true;
        }
        const auto& s = cemucore::snaps[addr];
        std::string out = "OK " + std::to_string(s.taken.size()) + " ";
        if (!s.taken.empty()) {
            char hex[3];
            for (uint8_t b : s.taken.back()) {
                std::snprintf(hex, sizeof hex, "%02X", b);
                out += hex;
            }
        }
        respond(out);
        return true;
    }
    if (command == "counts") {
        /* counts: "OK <cpu-cycles> addr=hits ..." */
        std::string out = "OK " + std::to_string(cemucore::sched_total_cycles());
        char item[48];
        for (const auto& entry : counters) {
            uint64_t hits = 0;
            cemucore::debug_hit_counter_get(entry.first, &hits);
            std::snprintf(item, sizeof item, " %06X=%llu", entry.first,
                          static_cast<unsigned long long>(hits));
            out += item;
        }
        respond(out);
        return true;
    }
    if (command == "screenshot" || command == "save-state") {
        std::string path;
        std::getline(input >> std::ws, path);
        if (path.empty()) {
            respond("ERR missing output path");
        } else if (command == "screenshot" ? writeScreenshot(path) :
                   cemucore::emu_save(cemucore::EMU_DATA_IMAGE, path.c_str())) {
            respond("OK " + command + " " + path);
        } else {
            respond("ERR failed to write " + path);
        }
        return true;
    }
    if (command == "send-file") {
        std::string location_text, path;
        input >> location_text;
        std::getline(input >> std::ws, path);
        int location;
        if (location_text == "ram") {
            location = cemucore::LINK_RAM;
        } else if (location_text == "archive") {
            location = cemucore::LINK_ARCH;
        } else {
            respond("ERR usage: send-file ram|archive <path>");
            return true;
        }
        if (!fileExists(path)) {
            respond("ERR input file does not exist: " + path);
        } else if (sendFile(path, location)) {
            respond("OK send-file " + path);
        } else {
            respond("ERR failed to send " + path);
        }
        return true;
    }
    if (command == "receive-file") {
        /* receive-file <name> <type-hex> <path>: copy a variable off the calculator */
        std::string name, type_text, path;
        uint32_t type;
        input >> name >> type_text;
        std::getline(input >> std::ws, path);
        if (name.empty() || path.empty() || !parseUnsigned(type_text, type, 16)) {
            respond("ERR usage: receive-file <name> <type-hex> <path>");
            return true;
        }
        cemucore::calc_var_t var;
        cemucore::vat_search_init(&var);
        bool found = false;
        while (cemucore::vat_search_next(&var)) {
            if (var.type == type && var.namelen == name.size() &&
                !std::memcmp(var.name, name.data(), name.size())) {
                found = true;
                break;
            }
        }
        if (!found) {
            respond("ERR no such variable " + name);
            return true;
        }
        std::ofstream file(path, std::ios::binary);
        file.write(reinterpret_cast<const char *>(var.data), var.size);
        respond(file.good() ? "OK receive-file " + std::to_string(var.size)
                            : std::string("ERR failed to write ") + path);
        return true;
    }
    if (command == "vars") {
        /* vars: every variable as name:type:size:archived */
        cemucore::calc_var_t var;
        cemucore::vat_search_init(&var);
        std::string out = "OK";
        while (cemucore::vat_search_next(&var)) {
            /* system variables have token bytes in their names: %XX */
            std::string name;
            for (unsigned i = 0; i < var.namelen; i++) {
                char c = static_cast<char>(var.name[i]);
                if (std::isalnum(static_cast<unsigned char>(c))) {
                    name += c;
                } else {
                    char esc[4];
                    std::snprintf(esc, sizeof esc, "%%%02X", var.name[i]);
                    name += esc;
                }
            }
            char item[32];
            std::snprintf(item, sizeof item, ":%02X:%u:%d", var.type,
                          static_cast<unsigned>(var.size), var.archived ? 1 : 0);
            out += " " + name + item;
        }
        respond(out);
        return true;
    }
    if (command == "profile") {
        /* profile <ms> <sample-us>: run, sampling the program counter every
         * sample-us microseconds; answers "OK <samples> addr=count ..." */
        std::string ms_text, us_text;
        uint32_t ms, us;
        if (!(input >> ms_text >> us_text) || !parseUnsigned(ms_text, ms) ||
            !parseUnsigned(us_text, us) || !us) {
            respond("ERR usage: profile <ms> <sample-us>");
            return true;
        }
        std::map<uint32_t, uint32_t> hist;
        cemucore::emu_set_run_rate(1000000);
        const uint64_t samples = static_cast<uint64_t>(ms) * 1000 / us;
        for (uint64_t i = 0; i < samples; i++) {
            cemucore::emu_run(us);
            uint32_t pc = cemucore::cpu.registers.PC;
            if (!cemucore::cpu.ADL) {
                pc = (static_cast<uint32_t>(cemucore::cpu.registers.MBASE) << 16) | (pc & 0xFFFF);
            }
            pc &= 0xFFFFFF;
            if (pc >= 0xE30800 && pc < 0xE30C00) {
                /* FamiCalc copies different routines into the cursor/SHA
                 * RAM at different times: tag the sample with the first
                 * byte of the routine that is loaded there right now */
                pc |= static_cast<uint32_t>(cemucore::mem_peek_byte(0xE30BC0)) << 24;
            }
            hist[pc]++;
        }
        cemucore::emu_set_run_rate(1000);
        std::string out = "OK " + std::to_string(samples);
        char item[32];
        for (const auto& entry : hist) {
            std::snprintf(item, sizeof item, " %X=%u", entry.first, entry.second);
            out += item;
        }
        respond(out);
        return true;
    }
    if (command == "reset") {
        cemucore::emu_reset();
        respond("OK reset");
        return true;
    }

    respond("ERR unknown command " + command);
    return true;
}
} // namespace

int main(int argc, char **argv)
{
    if (argc != 3 || (std::strcmp(argv[1], "--rom") && std::strcmp(argv[1], "--image"))) {
        std::cerr << "Usage: " << argv[0] << " (--rom <file> | --image <file>)\n";
        return EXIT_FAILURE;
    }
    const cemucore::emu_data_t type = std::strcmp(argv[1], "--rom")
        ? cemucore::EMU_DATA_IMAGE : cemucore::EMU_DATA_ROM;
    /* before emu_load: resets already touch the debugger's tables */
    cemucore::debug_init();
    if (cemucore::emu_load(type, argv[2]) != cemucore::EMU_STATE_VALID) {
        std::cerr << "Failed to load " << argv[2] << '\n';
        return EXIT_FAILURE;
    }
    /* Emulate the LCD controller's DMA and the LCD panel, like the real
     * hardware: screenshots then show what the panel shows (FamiCalc drives
     * it over SPI), and the DMA's memory accesses slow the CPU as they do on
     * a calculator. */
    cemucore::emu_set_lcd_dma(1);
    /* dbg_printf() output from test programs */
    cemucore::debug_flag(DBG_SOFT_COMMANDS, true);
    /* emu_run() counts in milliseconds of emulated time */
    cemucore::emu_set_run_rate(1000);

    respond("FC_HEADLESS_READY");

    std::string line;
    while (std::getline(std::cin, line) && runCommand(line)) {}

    cemucore::emu_exit();
    cemucore::asic_free();
    return EXIT_SUCCESS;
}
