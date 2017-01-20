# How do I compile and run this on Ubuntu 14.04?

## Install Stack.

Follow the [Stack documentation](https://docs.haskellstack.org/en/stable/README/).

## Install operating system dependencies.

```
sudo apt-get install libncursesw5-dev
```

The package libncursesw5-dev satisfies hscurses.
We need libncursesw5-dev (with the w) because
we use characters outside the ASCII character set.

## Run the interpreter.

If this is the first time you run this,
it will also install the application dependencies.

```
stack ghci
```

Then, enter either `webMain` for the web interface
or `dynastyMain` for the text interface.

# How do I generate the haddock of this project?

Install hscolour if you haven't do that:

```
stack install hscolour
```

To generate documentation:

```
stack haddock
```

The generated documentation path looks like this (the actual path on your machine may differ):

```
.stack-work/install/x86_64-linux/lts-6.26/7.10.3/doc/dynasty-0.0.0/index.html
```

# What do we plan to do?

(a.k.a. Roadmap)

- Show day using ncurses.
- Press n to go to next day.
- Show keyboard controls.
- Use Gregorian calendar.
- Show character name.
- Show character traits.
- Show character titles.
- Show character attributes.
- Run a Scotty web server instance at localhost:8008.
- Use the browser for user interface.

The URL /people shows a HTML page describing all people in the game.

Allow the user to configure the web server port via environment variable `HTTP_PORT`.

Show character location.

Show neighboring counties.

Define custom conditions and ordering for finding characters.

Find characters using custom conditions
- courtiers who have much gold but don't have any adult children, but will accept invitation to court (rich people that will inherit to their lieges)
- greedy envious deceitful foreign people with high intrigue
- martial leaders
- heirs who hate their lieges
- sort by plot power and distance, filter by distance
- married pairs where one has skills and the other can be killed
- kings who have less levies than i do and have less than half of the amount of gold i own

Remap keyboard controls.

Evaluate script.

Fire an event.

Find a wife.

Explain succession.

Show dynasty.

Plot.

Multiplayer server.

Save game.

Load game.

Start new game.

Generate maps and characters.

The server speed is the minimum of the players' speeds. If a player is responding to an event, the server pauses.

# Legal notices

Copyright 2017 Erik Dominikus

Licensed under Apache License Version 2.0
