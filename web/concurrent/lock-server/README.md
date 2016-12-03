# Lock server

The server stores named locks.

The locks are advisory.
Clients must cooperate in order for locking to be useful.

Locking complicates your system.
Before you try locking, try these simpler alternatives:

* Make a work coordinator that distributes non-conflicting tasks to worker processes.
* Hide the shared resource behind a process that presents a queue.
Here locking is limited in the process running the service.

## Operating manual

### Building the server

Build the entire project.
See the root project's readme.

### Starting the server from Gradle

The project uses Gradle application plugin.

```
JAVA_OPTS=??? gradle :concurrent:lock-server:run
```

### Starting the server outside Gradle

```
gradle installDist
```

That creates the shell script `build/install/lock-server/bin/lock-server`.

```
JAVA_OPTS='-Dport=PORT' PATH/lock-server
```

Replace `PORT` with a port number.
Replace `PATH` with the path to the script.

You must specify the port.
The server does not have a default port.

### Avoiding race conditions while restarting

Every time the server goes down, you must not restart it
before one period of the longest critical section in
all clients of the server has elapsed.
This is to ensure that all clients have assumed
that they don't own any locks.
