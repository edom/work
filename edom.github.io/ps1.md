---
title: Reverse-engineering PlayStation 1 3D games to control the camera
date: 2018-05-25 00:00 +0700
permalink: /ps1.html
---

We are interested in reverse-engineering these (mostly PlayStation 1) games:
Street Fighter EX series (EX Plus Alpha, EX2, EX3), Fighting Layer, Fighting EX Layer,
Tekken series from Tekken 3, Virtual Hiryu No Ken, Dead or Alive series,
Virtua Fighter series, Bloody Roar series.

- I want to move the camera arbitrarily in the PlayStation game "Street Fighter EX plus alpha".
    - The same technique might be applicable to other 3D PS1 games such as "Street Fighter EX2 plus", "Tekken 3", and "Fighting Layer" (Namco System 12, beefed-up PlayStation 1).
    - Interesting instructions: RTPT, CTC2.
        - I think that's the only way the game can render the graphics at 30 fps, given the hardware.
        - Need some knowledge of linear algebra and computer graphics.
    - Plan:
        - Find the address of the most often called RTPT instruction.
        - Find the CTC2 address that sets up the transformation matrices before that RTPT instruction.
        - Map the call graph.
        Find the bottom-most call that coincides with the rendering of one frame.
            - Insert breakpoint.
            - Go until return.
        - Use GDB.
        Turn on debug info.
        Disable optimization.
        Write some tracing code.
        Use GDB to toggle the code.
        Write a line to trace output file every time a CTC2 or RTPT instruction is executed.
        Every line contains register values (including program counter PC).
        - Add GDB script to run current function until it returns.
        Do this by putting a temporary breakpoint at `$ra`.
- How to use GDB to debug MAME?
    - PCSX is easy to debug because it is in C, uses global variables (such as `psxRegs`), and doesn't care about 100% faithful emulation.
    - MAME uses C++ and tries to emulate the whole hardware.
        - Can we supercompile MAME to make it faster?
        But how do we debug supercompiled programs?
        - We can make our own global variables.
        The source is open.
- https://en.wikipedia.org/wiki/PlayStation_(console)#Hardware
- https://en.wikipedia.org/wiki/PlayStation_technical_specifications
- http://phoboslab.org/log/2015/04/reverse-engineering-wipeout-psx
- Tools
    - `strings` from GNU binutils: `strings -t x FILE.bin`
    - `iat`: convert bin to iso
        - "Archive Manager" can open ISO
    - Does it work?
        - https://github.com/cebix/psximager
- technical documentation
    - https://www.zophar.net/fileuploads/2/10731bgqkx/playstation.htm
    - http://problemkaputt.de/psx-spx.htm
- Emulators
    - pcsx-reloaded
- Developing
    - SDKs
        - psxsdk
            - http://unhaut.x10host.com/psxsdk/doc/dir_000009.html
- Reverse engineering
    - https://reverseengineering.stackexchange.com/questions/1817/is-there-any-disassembler-to-rival-ida-pro
    - http://www.capstone-engine.org/
