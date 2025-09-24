module Meta where

import IMP.Syntax

-- | TODO
data Command
    = Help
    | Quit
    | Clear
    | Reset Aspect
    | Show (Either Aspect Option)
    | Load FilePath
    | Write FilePath
    | AST Element
    | Set Option
    | Unset Option

-- | TODO
data Aspect
    = Vars
    | Procs
    | Break
    | Trace
    | State

-- | TODO
data Element = Index Int | Input Construct

-- | Modifiable option in 'IMP.REPL.repl' through @:set@ and @:unset@.
data Option
    = Welcome String
    | Prompt String
    | Goodbye String
    | Verbose Int
    deriving (Eq, Ord, Show)

{-
-- parser for Unset option desugares to Set _defaults.option
-- probably remove Unset constructor
--? instantiate Command with Parses
-}
