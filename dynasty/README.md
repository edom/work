# How do I compile this on Ubuntu 14.04?

Install [Stack](https://docs.haskellstack.org/en/stable/README/).

Install development packages:

```
sudo apt-get install libncursesw5-dev
```

libncursesw5-dev satisfies hscurses.

# What do we plan to do?

(a.k.a. Roadmap)

- Show day using ncurses.
- Press n to go to next day.

Show keyboard controls.

Show character name.

Show character attributes.

Show character traits.

Show character titles.

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
