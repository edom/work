{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

module Sound.Hint
(
    -- * Error handling
    ioe
    , ioioe
    , ioeio
    , nmbei

    -- * Type-level comments
    , Positive
    , Nonnegative
    , Min
    , Max
    , Inner
    , Outer
    , Second
    , SecondPerSample(..)

    , PowerOfTwoLength
    , Phase
    , Sample
    , Carrier
    , Modulator

    , Init
    , Ratio

    , StepSize
)
where

import qualified System.IO.Error as Ioe

-- | Convert a 'Left' value into an 'IOError'.
ioe :: (Show e) => Either e a -> IO a
ioe = either (Ioe.ioError . Ioe.userError . show) return

-- | Execute the action and convert a 'Left' result into an 'IOError'.
ioioe :: (Show e) => IO (Either e a) -> IO a
ioioe = (>>= either (Ioe.ioError . Ioe.userError . show) return)

ioeio :: (Show e) => Either e (IO a) -> IO a
ioeio = either (Ioe.ioError . Ioe.userError . show) id

-- | Convert a negative 'Maybe' to an 'Either'.
nmbei :: Maybe e -> Either e ()
nmbei = maybe (Right ()) Left

newtype SecondPerSample a
    = MkSecondPerSample { _unSecondPerSample :: a }
    deriving (Read, Show)

type Positive a = a
type Nonnegative a = a
type Min a = a
type Max a = a
type Inner a = a
type Outer a = a
type Second a = a

type PowerOfTwoLength a = a

type Phase a = a
type Sample a = a

type Carrier a = a
type Modulator a = a

type Init a = a
type Ratio a = a

-- | Integration step size.
type StepSize a = a
