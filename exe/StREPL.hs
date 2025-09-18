module StREPL where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import System.Console.Haskeline hiding (display)

import System.Exit (exitFailure)

import Config
import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.State
import IMP.Statement
import IMP.Syntax

-- | Encapsulation of computation in 'IMP.REPL'.
type REPL = StateT Store (InputT IMP) -- CHECK: does this make sense?

data Setup = Setup
    { settings :: Settings IMP
    , prefs :: Prefs -- TODO: link documentation
    }

normal :: Setup
normal =
    Setup
        { settings = defaultSettings {historyFile = history}
        , prefs = defaultPrefs
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
    { _state :: State
    , _trace :: [Stm]
    , configs :: [Config] -- CHECK: handle invariants (contradicting configs, etc.)
    , multiline :: Maybe Int
    }

start :: Store
start =
    Store
        { _state = initial
        , _trace = []
        , configs = defaults
        , multiline = Nothing
        }

repl :: Setup -> Store -> IO ()
repl (Setup s p) store =
    do
        putStrLn welcome
        runExceptT $
            runInputTWithPrefs p s $
                execStateT loop store
        >>= either (\e -> print e >> exitFailure) (\_ -> putStrLn goodbye)

-- prepend new configs, always use first found value
--

-- | REPL loop that processes input and maintains interpreter state.
loop :: REPL ()
loop = do
    -- TODO:
    -- - use display
    -- - use prompt
    -- - use indent, handle multiline
    line <- lift . getInputLine $ ">" -- TODO: you'll know
    case line of
        Nothing -> return () -- ctrl-d, exit cleanly
        Just "" -> loop -- empty line, loop
        Just (':' : rest) -> handleMeta . normalizeMeta $ words rest
        Just input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                dispatch
                (parser "interactive" input)
        `catchError` \e -> case e of
            Empty -> outputln "" -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> throwError e -- unrecoverable, propagate
            Raised _ -> throwError e -- ''
            _ -> display e >> loop -- mistakes happen

-- | Lift computation from 'IMP.State.IMP' into 'REPL'.
liftIMP :: IMP a -> REPL a
liftIMP = lift . lift

-- | Process construct in environment, return updated environment.
dispatch :: Construct -> REPL ()
dispatch cnstr = do
    trace <- gets _trace
    state <- gets _state
    case cnstr of
        Statement stm -> do
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_trace = stm : trace, _state = state'}
        Arithmetic aexp -> display (evaluate state aexp)
        Boolean bexp -> outputln (if evaluate state bexp then "true" else "false")
        Whitespace -> return ()

-- | Help message displayed when user enters @:help@ metacommand.
helpMessage :: [String]
helpMessage =
    [ ":help / :?               Show this help message"
    , ":quit                    Quit REPL"
    , ":clear                   Clear screen"
    , ":reset [ASPECT]          Reset environment or specific aspect (vars, procs, break, trace)"
    , ":trace                   Show trace (executed statements)"
    , ":state                   Show state definitions (variables, procedures, break flag)"
    , ":load FILE               Interpret file and load resulting state"
    , ":write FILE              Write trace to file (relative to $PWD)"
    , ":ast (INPUT | #n)        Parse and display AST of input or n-th statement in trace"
    ]

-- | Expand metacommand abbreviations.
normalizeMeta :: [String] -> [String]
normalizeMeta ["?"] = ["help"]
normalizeMeta ["h"] = ["help"]
normalizeMeta ["q"] = ["quit"]
normalizeMeta ["c"] = ["clear"]
normalizeMeta ["t"] = ["trace"]
normalizeMeta ["s"] = ["state"]
normalizeMeta (w : ws)
    | w `elem` ["l", "load"] = ["load", it]
    | w `elem` ["w", "write"] = ["write", it]
    | w `elem` ["a", "ast"] = ["ast", it]
    | w `elem` ["r", "reset"] = ["reset", it]
    where
        it = unwords ws
normalizeMeta rest = rest

-- | Process metacommand in environment, continue loop or exit.
handleMeta :: [String] -> REPL ()
handleMeta meta = undefined
