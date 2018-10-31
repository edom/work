---
title: Learning to play mahjong
permalink: /mahjong.html
date: 2018-05-31 00:00:00 +0700
---

# Learning to play mahjong

The variant discussed here is classic Chinese mahjong.

## Basic facts

There are 136 mahjong tiles,
of which 52 are dealed to the players.
The remaining 84 forms the wall.
Thus, in a play, you only get at most 21 chances to change your hand,
that is 84 divided by 4 (the number of players).

If each player takes 15 seconds, then a hand
will take at most 4 x 15 x 21 = 1,260 seconds (21 minutes).

There are 136 tiles (not including 4 flower tiles and 4 season tiles):

There are 3 suits (bams, dots, and chars). Each suit has 9 classes. Each number has 4 instances. Thus each suit has 36 tiles. Thus there are 108 suit tiles.

There are 7 honors (4 winds and 3 dragons). Each honor has 4 instances. Thus there are 28 honor tiles.

There are 27 suit classes and 7 honor classes, giving a total of 34 classes.

Each tile class has 4 instances.

Each tile is equally probable to be drawn.

The probability of picking a tile class is 4/136. The odds of picking a tile class is 4:132.

## Number of chows, pungs, and kongs

The probability of drawing a pung:
34 * 4/136 * 3/135 * 2/134 = 1/3,015 (odd of 1 : 3,014, approx 1 : 3,000)

Each suit has 7 unique chows: 123, 234, 345, 456, 567, 678, 789.
There are 3 suits, so there are 21 unique chows.
Each tile has 4 instances.
Each chow has 4^3 instances.
The probability of drawing a chow: 21 * 4^3 / (136 * 135 * 134) = 28 / 51,255
(odd of 1 : 51,227, approx 1 : 51,000).

Pung is more likely than chow. Is this a calculation mistake?

The probability of drawing a kong (four identical tiles):
34 * 4/136 * 3/135 * 2/134 * 1/133 = 1/400,995 (odd of 1 : 400,994, approx 1 : 401,000)

The probability of drawing a pair:
34 * 4/136 * 3/135 = 1/45 (odd of 1 : 44)

The probability of drawing two tiles having the same class:
4/136 * 3/135 = 1/1,530 (odd of 1 : 1,529)

The probability of drawing two tiles having the same suit (3 suits):
3 * 36/136 * 35/135 = 7/34 (odd of 7 : 27, approx 1 : 4)

The probability of drawing a chow:
The probability of drawing n, n+1, n+2 unordered of the same suit, for n in 1 to 7:

3! * 3 * 7 * 4/136 * 4/135 * 4/134 = 56/17,085 (odd of 56 : 17,029, about 1 : 304)

The probability of drawing a pair, given that one tile has been drawn by another player:

Let each player draw exactly one tile. The probability of four players drawing the same tile:
34 * 4/136 * 3/135 * 2/134 * 1/133 = the probability of drawing a kong
