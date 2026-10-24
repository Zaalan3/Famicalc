#!/bin/bash
# Builds FamiCalc and puts it on the calculator in one go, printing only a
# few lines:
#   tools/deploy.sh [--no-test]
#   tools/deploy.sh --check    only reads the calculator's copy back and
#                              compares it with bin/FAMICALC.8xp
# 1. make with the v14.2 toolchain (v15.0 fails to link FamiCalc).
# 2. Unit tests (tests/unit) and PC tests (tests/pc/fctest.py all, on the new
#    bin/FAMICALC.8xp), unless --no-test; stops if one fails.
# 3. Keeps the build (+ .map) in ~/ti84ce/builds/famicalc/FAMICALC-<commit>.8xp
#    (<commit>-dirty when the code has uncommitted changes).
# 4. Waits up to 10 minutes for the calculator's home screen (the link only
#    works there), then reads its FAMICALC twice (they must match) and keeps
#    it in ~/ti84ce/builds/famicalc/calc/<date>-before-<commit>/.
# 5. Sends the build into Archive, reads it back and compares the program
#    (not the file header: its comment and archive flag differ).
# Everything else goes to obj/deploy.log.
set -u
cd "$(dirname "$0")/.."
LINK=${LINK:-$HOME/ti84ce/build/tools/ce-link/ce-link}
BUILDS=${BUILDS:-$HOME/ti84ce/builds/famicalc}
TEST=1
CHECK=0
while [ $# -gt 0 ]; do
    case $1 in
        --no-test) TEST=0 ;;
        --check) CHECK=1 ;;
        *) echo "usage: $0 [--no-test] [--check]"; exit 2 ;;
    esac
    shift
done
LOG=obj/deploy.log
TMP=obj/deploy
mkdir -p "$TMP"
fail() { echo "FAILED: $*  (details: $LOG)"; exit 1; }

# md5 of a .8xp's variables (type, name, data), without the file header.
prog_md5() {
    python3 -I - "$1" <<'EOF'
import hashlib, struct, sys
d = open(sys.argv[1], 'rb').read()
end = 55 + struct.unpack_from('<H', d, 53)[0]
pos, h = 55, hashlib.md5()
while pos < end:
    hlen, dlen = struct.unpack_from('<HH', d, pos)
    h.update(d[pos + 4:pos + 13])   # type + name
    pos += 2 + hlen + 2   # the header, then the data length again
    h.update(d[pos:pos + dlen])
    pos += dlen
print(h.hexdigest())
EOF
}

if [ $CHECK = 1 ]; then
    rm -f "$TMP/check.8xp"
    "$LINK" get FAMICALC "$TMP/check.8xp" >> "$LOG" 2>&1 || fail "reading FAMICALC"
    [ "$(prog_md5 "$TMP/check.8xp")" = "$(prog_md5 bin/FAMICALC.8xp)" ] \
        && echo "FAMICALC matches bin/FAMICALC.8xp ($(prog_md5 bin/FAMICALC.8xp | cut -c1-8))" \
        || fail "FAMICALC differs from bin/FAMICALC.8xp"
    exit 0
fi
: > "$LOG"

NAME=$(git rev-parse --short HEAD)
git diff --quiet HEAD -- src makefile || NAME=$NAME-dirty
export PATH=$HOME/ti84ce/toolchain/v14.2/CEdev/bin:$PATH
make >> "$LOG" 2>&1 || fail "build"

if [ $TEST = 1 ]; then
    make -B -C tests/unit >> "$LOG" 2>&1 || fail "unit tests: $(grep -a 'checks' "$LOG" | tail -1)"
    echo "Unit tests: $(grep -a 'checks' "$LOG" | tail -1)"
    make -C tests/pc >> "$LOG" 2>&1 || fail "building the PC harness"
    python3 tests/pc/fctest.py all >> "$LOG" 2>&1 \
        || fail "PC tests: $(grep -a '^FAIL' "$LOG" | head -3 | tr '\n' ' ')"
    echo "PC tests: $(grep -a 'passed$' "$LOG" | tail -1)"
fi

mkdir -p "$BUILDS"
[ -e "$BUILDS/FAMICALC-$NAME.8xp" ] \
    && [ "$(prog_md5 bin/FAMICALC.8xp)" != "$(prog_md5 "$BUILDS/FAMICALC-$NAME.8xp")" ] \
    && NAME=$NAME-$(date +%H%M)   # a dirty build that changed: don't overwrite the kept one
if [ ! -e "$BUILDS/FAMICALC-$NAME.8xp" ]; then
    cp bin/FAMICALC.8xp "$BUILDS/FAMICALC-$NAME.8xp"
    cp bin/FAMICALC.map "$BUILDS/FAMICALC-$NAME.map"
fi

# The calculator answers "info" anywhere, but sends and reads need the home
# screen (ce-link's ready check).
for try in $(seq 1 60); do
    "$LINK" ls > "$TMP/ls.txt" 2>> "$LOG" && break
    [ $try = 1 ] && echo "Waiting for the calculator's home screen..."
    [ $try = 60 ] && fail "the calculator didn't get to the home screen in 10 minutes"
    sleep 10
done

if grep -q '^FAMICALC ' "$TMP/ls.txt"; then
    rm -f "$TMP/old1.8xp" "$TMP/old2.8xp"
    "$LINK" get FAMICALC "$TMP/old1.8xp" >> "$LOG" 2>&1 || fail "reading FAMICALC"
    "$LINK" get FAMICALC "$TMP/old2.8xp" >> "$LOG" 2>&1 || fail "reading FAMICALC"
    [ "$(prog_md5 "$TMP/old1.8xp")" = "$(prog_md5 "$TMP/old2.8xp")" ] \
        || fail "the two reads of FAMICALC differ; run again"
    BACKUP=$BUILDS/calc/$(date +%Y-%m-%d-%H%M)-before-$NAME
    mkdir -p "$BACKUP" && cp "$TMP/old1.8xp" "$BACKUP/FAMICALC.8xp"
    OLD=$(prog_md5 "$TMP/old1.8xp")
    for f in "$BUILDS"/FAMICALC-*.8xp; do   # which kept build it was
        [ "$(prog_md5 "$f")" = "$OLD" ] && WAS=$(basename "$f" .8xp | cut -d- -f2-) && break
    done
    echo "Old copy: $BACKUP (${WAS:-not a kept build})"
else
    echo "No FAMICALC on the calculator yet"
fi

"$LINK" send -a bin/FAMICALC.8xp >> "$LOG" 2>&1 || fail "send"
WANT=$(prog_md5 bin/FAMICALC.8xp)
GOT=none
for try in 1 2 3 4 5 6; do
    rm -f "$TMP/check.8xp"
    if "$LINK" get FAMICALC "$TMP/check.8xp" >> "$LOG" 2>&1; then
        GOT=$(prog_md5 "$TMP/check.8xp")
        [ "$GOT" = "$WANT" ] && break
    else
        sleep 5
    fi
done
[ "$GOT" = "$WANT" ] || fail "read back md5 $GOT, sent $WANT (check: tools/deploy.sh --check)"

echo "Sent $NAME as FAMICALC (Archive): $(stat -c %s bin/FAMICALC.8xp) B, md5 ${WANT:0:8}, read back OK"
