---
title: Standard MIDI file
permalink: /smf.html
date: 2017-09-24 23:39 +0700
---

Summary of [David Back's Standard MIDI-File Format Spec. 1.1](http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html):

```
endianness of uint32 is big

a File is
    1 Head
    (0 or more) (Track or Chunk_alien)

a Head is
    a Chunk of type "MThd" of body Head_body

a Chunk of type T of body B is
    type: byte[4] = T
    length: uint32
    body: B
        where the length of B in bytes is <length>

a Chunk_alien is
    type: byte[4]
    length: uint32
    body: byte[length]

a Head_body is

    nbytes: uint32 = 6

    format: uint16 where
        0 means single-track
        1 means multi-track

    ntracks: uint16
        should match how many Tracks are in the file

    division: signed int16
        ticks per quarter note
        must be positive

a Track is
    a Chunk of type "MTrk" of body MTrk_body

a MTrk_body is
    0 or more MTrk_event

a MTrk_event is

    delta_time: variable length integer
        = the time of this event - the time of the previous event in the same track
        in number of ticks (as defined in Head_body)

    event: Midi_event or Sysex_event or Meta_event
        Midi_event has 'running status' (parsing is stateful)
```
