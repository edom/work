{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sound
(
    -- * Reexports
    -- $reexports
    module Control.Monad
    , module Data.Complex
    , module Data.Int
    -- * Input, output, and conversion
    , module Sound.Io.Snd
    , module Sound.Io
    , module Sound.IoPtr
    , module Sound.Portaudio
    -- * Abstraction ladder
    , module Sound.Class
    , module Sound.Abstract
    , module Sound.Compile
    , module Sound.InfList
    , module Sound.List
    , module Sound.GenBody
    -- * Loop body abstraction ladder
    , module Sound.Endo
    -- * Stream abstraction ladder, signal description and manipulation, additive synthesis
    , module Sound.GeneratorContinuation
    -- * Unclear
    , module Sound.Buffer
    , module Sound.Pair
    , module Sound.Function
    -- * Time, tempo
    , module Sound.Time
    , module Sound.Tempo
    -- * Sampling
    , module Sound.Sample
    -- * Decibels, contours, envelopes, limiting, clamping
    , module Sound.Amplitude
    , module Sound.Ramp
    -- * Wavetable
    , module Sound.Table
    -- * White noise
    , module Sound.Random
    -- * Fourier transform
    , module Sound.Fourier
    -- * Frequency modulation
    , module Sound.Fm
    -- * Integration
    , module Sound.Int
    -- * Instrument
    , module Sound.Perform
    -- * MIDI input
    , module Sound.Midi
    -- * Low-level operations
    , module Sound.Ptr
    -- * Error handling, type hints
    , module Sound.Hint
    -- * Failed experiments
    , module Sound.IoFail
    , module Sound.IoSox
    , module Sound.Stream
    , module Sound.StreamVector
    -- * Signal description and operations
    -- ** Filters: zeros and poles
    , module Sound.Filter
    -- ** Waveshaping
    -- $waveshaping
)
where

import Data.Complex
import Data.Int

import Control.Applicative
import Control.Monad

import Sound.Abstract
import Sound.Amplitude
import Sound.Buffer
import Sound.Class
import Sound.Compile
import Sound.Endo
import Sound.Filter
import Sound.Fourier
import Sound.Fm
import Sound.Function
import Sound.GenBody
import Sound.GeneratorContinuation
import Sound.Hint
import Sound.InfList
import Sound.Int
import Sound.Io
import Sound.Io.Snd ()
import Sound.IoFail
import Sound.IoPtr
import Sound.IoSox
import Sound.List ()
import Sound.Midi
import Sound.Pair
import Sound.Perform
import Sound.Portaudio ()
import Sound.Ptr
import Sound.Ramp
import Sound.Random
import Sound.Sample
import Sound.Stream
import Sound.StreamVector
import Sound.Table
import Sound.Tempo
import Sound.Time

import qualified Prelude as P

{- $reexports
If you use this module unqualified, you have to hide Prelude imports to avoid name clashes.
The portable way to do that is by adding this import to your code:

@
import Prelude ()
@

This module replaces 'P..' and 'P.id' from "Prelude"
with '.' and 'id' from "Control.Category".
This module hides 'P.seq' from "Prelude".

This module reexports everything else from all modules listed here.
-}

{- $waveshaping
Waveshaping is just function application or composition.
-}
