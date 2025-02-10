# FamiCalc

NES Emulator for the TI-84 Plus CE / TI-83 Premium CE. 

![FamiCalc](https://raw.githubusercontent.com/Zaalan3/Famicalc/main/capture.png)

# Installing

Use TI Connect CE or TILP to send FAMICALC.8xp to your calculator. Releases aren't available yet, as Famicalc is still under development, but you can easily build the program from the source code by following the instructions below.

If your calculator has OS version 5.5 or higher, follow the [instructions here](https://yvantt.github.io/arTIfiCE/) to run assembly programs.

Otherwise, do the following: 
1. Press [2nd]+[0] to open the Catalog
2. Select Asm(
3. Press [prgm] 
4. Select FAMICALC
5. Press [enter] 

# Converting .NES files to Appvars 

### Experimental Web-Based Converter 

Try out this experimental web-based ROM converter from [pixl8dev](https://github.com/pixl8dev)!

https://wary-fir-story.glitch.me

### Python Converter 

To convert the appvars needed to run this project, navigate to the **tools** folder and run **converter.pyw**. 
#### Linux:
Run this command in the terminal after navigating to the **tools** folder: ```sudo apt install python3-tk -y && sudo apt install python3.11 && python3.11 converter.pyw```

#### Windows:
After installing Python, right-click the file and select **Edit with IDLE**, then, after the editor opens, use the keyboard shortcut Fn+F5.


FamiCalc currently supports Mapper 0 and Mapper 2 games only.

# Controls

- A=> [2nd]
- B=> [Alpha]
- Start=> [Mode] 
- Select => [XTθn]

- Exit Program => [Del]

# How to Build

#### Windows:
This project was compiled with [The latest version of the CE Toolchain](https://github.com/CE-Programming/toolchain/releases). Navigate to the topmost folder(where the makefile is) and run *make*.

#### Linux (Ubuntu/Debian-based):
1. Open a terminal and run ```bash <(curl -s https://raw.githubusercontent.com/pixl8dev/cedevpath/refs/heads/main/cedevpath.sh)``` to install CE-Dev to PATH. (Make sure you're in your home directory!)
2. Run ```git clone https://github.com/Zaalan3/Famicalc.git && cd Famicalc```
4. Run ```make```
5. Your fresh Famicalc build will appear in the **bin** folder under the name FAMICALC.8xp.


Alternatively, you can build this project with [Nix](https://nixos.org/) using the `nix build` command.
