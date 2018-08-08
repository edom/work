---
title: Running X client applications on Docker on Linux
permalink: /dockerx.html
date: 2016-05-02 03:31:20 +0700
---

```
docker \
    -e DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
    -u <user> <image> <command>
```

Replace `<user>` with a non-root user.
You need a non-root user because the X server rejects connection from the root user by default.
You can change this with `xhost`, but it's better to connect with a non-root user.

The `<command>` argument is optional.

The `-e DISPLAY` parameter reexports the `DISPLAY` environment variable to the application inside the container.
X client applications will read from this environment variable to determine which server to connect to.

The `-v HOST:CONT:ro` option mounts `HOST` directory to `CONT` directory read-only.
This is so that the application in the container can connect to the host X server's Unix socket.

On Linux, display `:0` corresponds to the Unix socket `/tmp/.X11-unix/X0`.
Everyone who can connect to that Unix socket will
be able to run X client applications on the machine;
it is not specific to Docker.

If X complains about shared memory, try:

```
docker \
    -e DISPLAY=unix$DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
    -u <user> <image> <command>
```
