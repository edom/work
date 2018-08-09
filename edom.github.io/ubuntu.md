---
title: Administering Ubuntu
permalink: /ubuntu.html
date: 2018-08-10 03:49 +0700
---

- Why use swap partitions and not swap files?
    - Defragmenting swap files might have undesirable effects.
        - https://lwn.net/Articles/317787/
- ext4 defragmentation tools
    - https://askubuntu.com/questions/221079/how-to-defrag-an-ext4-filesystem
        - e2freefrag DEV, e4defrag -c FILE
- SATA 3 Gbps controller problem?
    - https://github.com/zfsonlinux/zfs/issues/4873
- Security
    - Isolating (sandboxing) an application
        - Creating a dedicated limited user for an application
            - `adduser --system`, or just use the `nobody` user?
    - Many methods of isolation
        - https://unix.stackexchange.com/questions/384117/linux-isolate-process-without-containers
        - https://www.engineyard.com/blog/linux-containers-isolation
        - https://opensourceforu.com/2016/07/many-approaches-sandboxing-linux/
- Why isn't ssh fail2ban the default?
Why is security not the default?
