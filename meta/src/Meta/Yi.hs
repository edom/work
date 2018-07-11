{- |
* 2008, article, Jean-Philippe Bernardy, \"Yi: an editor in Haskell for Haskell\" <http://publications.lib.chalmers.se/records/fulltext/local_72549.pdf>

    * Yi is a text editor implemented in Haskell and, more importantly, extensible in Haskell.
    It is structured around four embedded DSLs:

        * BufferM A DSL for all buffer-local operations, like insertion and deletion of text, and annotation of buffer contents.
        It can be understood as a monad that encapsulates the state of one buffer.

        * EditorM A DSL for editor-level operations, e.g., opening and closing windows and buffers.
        Operations involving more than one buffer are handled at this level too.

        * YiM A DSL for IO-level operations.
        There, one can operate on files, processes, etc.
        This is the only level where IO can be done.

        * KeymapM Key-binding descriptions.
        The structure of this DSL closely follows that of classic parser-combinator libraries.
        The semantics are a bit different though: the intention is to map a stream of input events to a stream of actions, instead of producing a single result.
        The actions of the output stream can come from any of the above DSLs.

* Problems

    * Lack of documentation

        * Its Haddock documentation is sparse.

        * The only way to learn it is by scrounging the developers' configurations.

            * <https://github.com/ethercrow/yi-config>

            * <https://github.com/search?l=Haskell&q=yi-config&type=Repositories>

    * API design issues

        * So many imports are required to write a configuration.
        Need a high-level module.

        * Perhaps BufferM, EditorM, YiM should be type classes.
        See the \"Actions\" section in this document.

        * What is the semantics?
        What are the laws?
        See Hughes's 1995 article about pretty printing combinators.
        Can we make text editor combinators?

* How do I make Yi do these things?

    * When I press F4 or Ctrl+Q, quit the editor.

    * When I press Ctrl+P, let me type a string S.
    As I type, recursively find every path F such that S is a /subsequence/ (not substring) of F.
    This is like Visual Studio Code. This is enhanced vim @:b[uffer]@ command.

        * <https://en.wikipedia.org/wiki/Subsequence>

    * When I press F2 or Ctrl+S, save the current file.

    * Recursively grep current word under cursor in all files, ignoring non-interesting files.

    * Save things automatically.

        * Make backups periodically.
        * Save the file every a few seconds or when the window is closed.
        * Call @sync@ every a few seconds or when the window is closed.
        * But isn't this basically a partial reimplementation of Git?

    * Remove trailing spaces when saving.

    * Restarting the editor should not affect the ability to undo file changes.

        * I have a bad feeling: I think we will end up reimplementing Git if we go down this path.

* See also

    * <http://www.nobugs.org/developer/yi/>

    * <https://www.reddit.com/r/haskell/comments/55igdm/what_would_you_want_to_see_in_a_yi_tutorial/>

    * <https://wiki.haskell.org/Yi>
-}
module Meta.Yi (
    -- * Starting the editor
    startEditor
    -- * Configuration
    , Config
    , makeConfig
    , ConfigM
    -- ** Picking a front-end
    , configureVty
    -- ** Binding keys to actions
    , globalBindKeys
    , KeymapM
    -- * Matching keys
    , Event
    , match
    , matchAny
    -- ** Keys
    , char
    , func
    -- ** Modifiers
    , ctrl
    -- * Actions
    -- $action
    , Y.Action(..)
    -- ** Editor actions
    , quitEditor
    -- ** Buffer actions
    , YBM.insertB
    -- * Preset configurations
    , YVI.configureVim
    , YH.configureHaskellMode
    , YM.configureMiscModes
    -- * Other reexports
    -- ** Perhaps better hidden
    , S.execStateT
    , YCT.runConfigM
    , Y.defaultConfig
    , Y.modelessKeymapSet
    , YI.MonadInteract(..)
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad.State.Lazy as S
import qualified Yi as Y
import qualified Yi.Buffer.Misc as YBM
import qualified Yi.Config.Default.HaskellMode as YH
import qualified Yi.Config.Default.MiscModes as YM
import qualified Yi.Config.Default.Vim as YVI
import qualified Yi.Config.Default.Vty as YVT
import qualified Yi.Config.Simple as YCS
import qualified Yi.Config.Simple.Types as YCT
import qualified Yi.Core as YC
import qualified Yi.Interact as YI
import qualified Yi.Keymap.Keys as YKK

{- $action
Currently 'Y.Action' is defined as:

@
data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)

newtype YiM a = YiM (ReaderT Yi IO a)
newtype EditorM a = EditorM (ReaderT Config (State Editor) a)
newtype BufferM a = BufferM (ReaderT Window (State FBuffer) a)
@

Perhaps YiM, EditorM, BufferM, and Action should be type classes.

@
class (Monad m) => MonadYi m where
    getYi :: m Yi

class (Monad m) => MonadEditor m where
    getConfig :: m Config
    getEditor :: m Editor
    setEditor :: Editor -> m ()

class (Monad m) => MonadBuffer m where
    getWindow :: m Window
    getBuffer :: m FBuffer
    setBuffer :: FBuffer -> m ()
@

We can also add a parameter to 'Y.Action' to move the @forall@ out.

@
data ActionM a = YiA (YiM a)
               | EditorA (EditorM a)
               | BufferA (BufferM a)

type Action = forall a. ActionM a
@
-}

{- |
An inhabitant of @KeymapM a@ describes how to match a key sequence and what to do if that sequence is matched.

KeymapM has an instance of 'Y.MonadInteract'.
-}
type KeymapM a = Y.KeymapM a

{- |
An inhabitant of @ConfigM a@ describes how to modify a 'Config'.

ConfigM has a 'Monad' instance.
-}
type ConfigM a = YCT.ConfigM a

-- | This tells Yi to use the vty (virtual teletype a.k.a. text user interface a.k.a. console a.k.a. terminal) front-end.
configureVty :: ConfigM ()
configureVty = YVT.configureVty

{- |
Add a global (mode-independent?) key binding.
-}
globalBindKeys :: KeymapM () -> ConfigM ()
globalBindKeys = YCS.globalBindKeys

-- | An 'Event' is a key and some modifiers.
type Event = Y.Event

{- |
Match the key.

The meaning of @match k >> react@ is:
if the user presses k, then proceed to @react@;
otherwise don't continue (akin to throwing an exception).

To match a chord (a key sequence), use an expression like the following:

@
do
    _ <- 'match' ('char' \'a\')
    _ <- 'match' ('char' \'b\')
    'quitEditor'
@

The above means \"if the user presses @a@ and @b@ in succession, then quit the editor\".
-}
match :: Event -> KeymapM Event
match = Y.event

-- | This matches any key.
matchAny :: KeymapM Event
matchAny = Y.anyEvent

-- | An ordinary key. This assumes that Caps Lock is off.
char :: Char -> Event
char = YKK.char

-- | A function key.
func :: Int -> Event
func = Y.spec . Y.KFun

-- | @'ctrl' e@ is @e@ with Ctrl key down.
ctrl :: Event -> Event
ctrl = Y.ctrl

-- | Quit the editor.
quitEditor :: KeymapM ()
quitEditor = Y.write (Y.YiA YC.quitEditor)

{- |
See 'makeConfig' for how to make a 'Config'.
-}
startEditor
    :: Config
    -> IO ()

startEditor conf = YC.startEditor conf Nothing

-- | See also 'Y.Config' from "Yi".
type Config = Y.Config

{- |
Modify the default configuration using the computation.
-}
makeConfig :: ConfigM a -> IO Config
makeConfig com = S.execStateT (YCT.runConfigM com) Y.defaultConfig
