module StREPL where

import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Except as Except
import qualified System.Console.Haskeline as Haskeline

import Control.Monad.Trans.Class (lift)
import System.Exit (exitFailure)

import Config
import IMP.Exception
import IMP.State
import IMP.Syntax

-- | Encapsulation of computation in 'IMP.REPL'.
type REPL = State.StateT Store (Haskeline.InputT IMP) -- CHECK: does this make sense?

data Setup = Setup
    { settings :: Haskeline.Settings IMP
    , prefs :: Haskeline.Prefs -- TODO: link documentation
    }

normal :: Setup
normal =
    Setup
        { settings = Haskeline.defaultSettings {Haskeline.historyFile = history}
        , prefs = Haskeline.defaultPrefs
        }

data Config
    = Welcome String
    | Prompt String
    | Goodbye String
    | Verbose Int -- INFO: non-negative integer!
    | Profile
    deriving (Eq, Ord, Show)

defaults :: [Config]
defaults =
    [ Welcome welcome
    , Prompt prompt
    , Goodbye goodbye
    , Verbose 1
    ]

-- | Encapsulation of state in 'IMP.REPL'.
data Store = Store
    { defs :: State
    , trace :: [Stm]
    , configs :: [Config] -- CHECK: handle invariants (contradicting configs, etc.)
    , multiline :: Maybe Int
    }

start :: Store
start =
    Store
        { defs = initial
        , trace = []
        , configs = defaults
        , multiline = Nothing
        }

repl :: Setup -> Store -> IO ()
repl (Setup s p) store =
    do
        putStrLn welcome
        Except.runExceptT $
            Haskeline.runInputTWithPrefs p s $
                State.execStateT loop store
        >>= either (\e -> print e >> exitFailure) (\_ -> putStrLn goodbye)

-- | REPL loop that processes input and maintains interpreter state.
loop :: REPL ()
loop = do
    -- TODO:
    -- - use display
    -- - use prompt
    -- - use indent, handle multiline
    line <- lift . Haskeline.getInputLine $ ">" -- TODO: you'll know
    return ()

-- prepend new configs, always use first found value
--
