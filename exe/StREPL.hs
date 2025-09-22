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
-- type REPL = StateT Store (ExceptT Exception (InputT IMP))
type REPL = StateT Store (InputT IMP)

-- | TODO
data Setup = Setup
    { settings :: Settings IMP
    , prefs :: Prefs -- TODO: link documentation
    }

-- | TODO
setup :: FilePath -> IO Setup
setup path = do
    prefs' <- readPrefs path
    return
        Setup
            { settings = defaultSettings {historyFile = history}
            , prefs = prefs'
            }

-- | TODO
data Option
    = Welcome String
    | Prompt String
    | Goodbye String
    | Verbose Int -- INFO: non-negative integer!
    | Profile
    deriving (Eq, Ord, Show)

-- | TODO
defaults :: [Option]
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
    , _defaults :: [Option]
    , _welcome :: String
    , _prompt :: String
    , _goodbye :: String
    , _verbose :: Int -- TODO: explain meaning of value here
    , _multiline :: Maybe Int
    }

-- | TODO
start :: Store
start =
    Store
        { _state = initial
        , _trace = []
        , _defaults = defaults
        , _welcome = welcome
        , _prompt = prompt
        , _goodbye = goodbye
        , _verbose = 1
        , _multiline = Nothing
        }

-- | TODO
repl :: Setup -> Store -> IO ()
repl (Setup s p) store =
    do
        putStrLn $ _welcome store
        runExceptT $
            runInputTWithPrefs p s $
                execStateT loop store
        >>= either (\e -> print e >> exitFailure) (\_ -> putStrLn goodbye)

-- | REPL loop that processes input and maintains interpreter state.
loop :: REPL ()
loop = do
    prompt' <- gets _prompt
    multi <- gets _multiline
    line <- lift . getInputLine $ case multi of
        Nothing -> prompt' ++ ">"
        Just i -> replicate (length prompt') '.' ++ replicate i '>'
    case line of
        Nothing -> return () -- ctrl-d, exit cleanly
        Just "" -> loop -- empty line, loop
        Just (':' : rest) -> handleMeta . normalizeMeta $ words rest
        Just input -> do
            cnstr <-
                liftIMP $
                    either
                        (\e -> throwError . ParseFail $ unlines [input, show e])
                        return
                        (parser "interactive" input)
                        `catchError` \e -> case e of
                            Empty -> return Whitespace -- ctrl-d during read, flush line and exit cleanly
                            AssertFail _ -> throwError e -- unrecoverable, propagate
                            Raised _ -> throwError e -- ''
                            _ -> display e >> return Whitespace -- mistakes happen
            dispatch cnstr
            loop

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
