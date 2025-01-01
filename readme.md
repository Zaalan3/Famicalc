# FamiCalc

NES Emulator for the TI-84 Plus CE / TI-83 Premium CE. 

![FamiCalc](https://raw.githubusercontent.com/Zaalan3/Famicalc/main/capture.png)

# Installing

Use TI Connect CE to send FAMICALC.8xp to your calculator. 

If your calculator has OS version 5.5 or higher, follow the [instructions here](https://yvantt.github.io/arTIfiCE/) to run assembly programs.

Otherwise, do the following: 
1. Press [2nd]+[0] to open the Catalog
2. Select Asm(
3. Press [prgm] 
4. Select FAMICALC
5. Press [enter] 

# Converting .NES files to Appvars 

To convert the appvars needed to run this project, navigate to the **tools** folder and run **converter.pyw**. 

FamiCalc currently supports NROM (Mapper 0) games only, at this moment in time. 

# Controls

- A=> [2nd]
- B=> [Alpha]
- Start=> [Mode] 
- Select => [XTθn]

- Exit Program => [Del]

# How to Build

This project was compiled with [The latest version of the CE Toolchain](https://github.com/CE-Programming/toolchain/releases). Navigate to the topmost folder(where the makefile is) and run *make*.


