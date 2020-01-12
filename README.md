# CPS_MiSTer

Capcom Play System (CPS) core for MiSTer.


# Status
- NO video output yet! But the Verilog for tile display is in progress.
- Sprites will need to use quite a lot of BRAM (384KB or more), which will just about fit on the FPGA on the DE10 Nano.
- FX68K CPU is running code from SDRAM now (SF2 being used as the main test ROM).
- T80 (Z80) CPU is running from BRAM atm, but sound is working OK.
- ADPCM ROM will need to be moved into SDRAM later, to free up more on-chip mem.
- Player controls (via MiSTer USB / HPS) appear to be working.


# Credits

MiSTer project founded and maintained by Sorgelig.
FX68K core by Jorge Cwik.
T80 core by MikeJ, with additions by TobiFlex, Sean Riddle, and Sorgelig.
jt51 core by Jotego.
Video timing logic (cps_video_beam and cps_asram_ctrl) by Frenchshark.
Redrawn CPS2 schematics by Loïc *WydD* Petit have been a big help.
Help and support by dentnz, Furrtek, laxer3a, bhamadicharef, and many others. ;)
