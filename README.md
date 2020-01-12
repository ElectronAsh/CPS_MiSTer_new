# GBA_MiSTer
GBA for MiSTer

# HW Requirements/Features
The games can run from a naked DE10-Nano with the build-in DDR-RAM.
However, using SDRAM is highly recommended, as some games may slowdown or loose sync when using DDR-RAM.

When using SDRAM, it requires 32MB SDRAM for games less than 32MB. 32MB games require either 64MB or 128MB module.
SDRAM will be automatically used when available.

# Bios
Opensource Bios from Normmatt is included, however it has issues with some games.
Original GBA BIOS can be placed to GBA folder with name boot.rom

PLEASE do not report errors without testing with the original BIOS

# Games with crashes/hang
- Colin McRae Rally 2.0 hangs when going into race

- Madden06/07: crash at coin toss

- Sennen Kazoku: hang on first screen
- Starsky & Hutch: crash going ingame

- TOCA World Touring Cars: hangs going into race

# Games that are unplayable because of catridge hardware missing
- Boktai 1/2/Shin Bokura no Taiyou(Japanese Boktai)
- Warioware Twisted
- Yoshi's Universal Gravitation

# Status
~1600 games tested until ingame:
- 99% without major issues (no crash, playable)

# Features
- all videomodes including affine and special effects
- all soundchannels
- saving as in GBA
- Savestates
- FastForward - speed up game by factor 2-4
- CPU Turbomode - give games additional CPU power
- Flickerblend - turn on for games like F-Zero, Mario Kart or NES Classics to prevent flickering effects
- Spritelimit - turn on to prevent wrong sprites for games that rely on the limit (opt-in because not 100% accurate)

# Savestates
Core provides 4 slots to save the state. The first slot gets saved to disk and automatically loaded (but not applied)
upon next load of game. Rest 3 slots are residing only in memory for temporary use.
First slot save/restore is available from OSD as well. 


Hot keys for save states:
- Alt-F1..F4 - save the state
- F1...F4 - restore


# Not included
- Multiplayer features like Serial
- Tilt/Gyro/Rumble/Sun sensor)
- RTC

# Accuracy

There is great testsuite you can get from here: https://github.com/mgba-emu/suite
It tests out correct Memory, Timer, DMA, CPU, BIOS behavior and also instruction timing. It works 100% on the real GBA.
The suite itself has several thousand single tests. Here is a comparison with mGBA, VBA-M and Higan

Testname | TestCount | Mister GBA| mGBA | VBA-M | Higan
---------|-----------|-----------|------|-------|-------
Memory   |      1552 |  1552     | 1552 |  1337 | 1552
IOREAD   |       123 |   123     |  116 |   100 |  123
Timing   |      1660 |  1554     | 1520 |   628 | 1424
Timer    |       936 |   445     |  511 |   440 |  464
Carry    |        93 |    93     |   93 |    93 |   93
BIOSMath |       625 |   625     |  625 |   425 |  625
DMATests |      1256 |  1212     | 1232 |  1008 | 1064
