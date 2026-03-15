module Main where

import Control.Exception (bracket)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (execStateT, modify)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (IOMode (WriteMode), hClose, openFile, stdout, withFile)
import Test.Tasty
import Test.Tasty.HUnit

import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset
import REPL.State

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "IMP Tests"
        [ testParse
        , testExec
        ]

testParse :: TestTree
testParse =
    testGroup
        "Parse"
        [ testParseStm
        , testParseAexp
        , testParseBexp
        , testParseConstruct
        , testParseCommand
        ]

testExec :: TestTree
testExec =
    testGroup
        "Exec"
        [ testExecStm
        , testExecAexp
        , testExecBexp
        , testExecCommand
        ]

testParseStm :: TestTree
testParseStm =
    testGroup
        "Statement"
        [ assertParseStm "skip" Skip
        , assertParseStm "x := 1" (VarDef "x" Def (Val 1))
        , assertParseStm "(skip; skip)" (Seq Skip Skip)
        , assertParseStm "if true then skip end" (IfElse (Lit True) Skip Skip)
        , assertParseStm "if true then skip else skip end" (IfElse (Lit True) Skip Skip)
        , assertParseStm
            "if true then skip else if true then skip end end"
            (IfElse (Lit True) Skip (IfElse (Lit True) Skip Skip))
        , assertParseStm
            "while false do if true then skip else if true then skip end end end"
            (While (Lit False) (IfElse (Lit True) Skip (IfElse (Lit True) Skip Skip)))
        , {-
          , assertParseStm
              "if true then skip else if true then skip end"
              (IfElse (Lit True) Skip (IfElse (Lit True) Skip Skip))
          , assertParseStm
              "while false do if true then skip else if true then skip end end"
              (While (Lit False) (IfElse (Lit True) Skip (IfElse (Lit True) Skip Skip)))
          -}
          assertParseStm "while true do skip end" (While (Lit True) Skip)
        , assertParseStm "print 1" (Print (Val 1))
        , assertParseStm "read x" (Read "x")
        , assertParseStm "let x := 1 in skip end" (Local "x" (Val 1) Skip)
        , assertParseStm
            "x := 1 par y := 2"
            (Par (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2)))
        , assertParseStm
            "x := 1 [] y := 2"
            (NonDet (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2)))
        , assertParseStm
            "skip par skip; skip [] skip"
            (Seq (Par Skip Skip) (NonDet Skip Skip))
        , assertParseStm
            "x := time (a := 1; b := 2)"
            (VarDef "x" Def (Time (Seq (VarDef "a" Def (Val 1)) (VarDef "b" Def (Val 2)))))
        , assertParseStm
            "procedure foo(;) begin skip end; foo(;)"
            (Seq (ProcDef "foo" [] [] Skip) (ProcInvoc "foo" [] []))
        , assertParseStm
            "do x := 1 until false end"
            (Seq (VarDef "x" Def (Val 1)) (While (Not (Lit False)) (VarDef "x" Def (Val 1))))
        , assertParseStm
            "for i := 0 to 3 do skip end"
            (Local "i" (Val 0) (While (Rel Lt (Var "i") (Val 3)) (Seq Skip (VarDef "i" Inc (Val 1)))))
        , assertParseStm
            "repeat 4 times skip end"
            (Local "_times" (Val 0) (While (Rel Lt (Var "_times") (Val 4)) (Seq Skip (VarDef "_times" Inc (Val 1)))))
        , assertParseStm
            "revert x := 1 if true"
            (Revert (VarDef "x" Def (Val 1)) (Lit True))
        , assertParseStm "x := 1 + 2 * 3" (VarDef "x" Def (Bin Add (Val 1) (Bin Mul (Val 2) (Val 3))))
        , assertParseStm "x := (1 + 2) * 3" (VarDef "x" Def (Bin Mul (Bin Add (Val 1) (Val 2)) (Val 3)))
        , assertParseStm
            "x := 1 [] y := 2 par z := 3"
            (Par (NonDet (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2))) (VarDef "z" Def (Val 3)))
        ]

testParseAexp :: TestTree
testParseAexp =
    testGroup
        "Arithmetic Expression"
        [ assertParseConstruct "(1)" $ Arithmetic (Val 1)
        , assertParseConstruct "x" $ Arithmetic (Var "x")
        , assertParseConstruct "1 + 2" $ Arithmetic (Bin Add (Val 1) (Val 2))
        , assertParseConstruct "1 - 2" $ Arithmetic (Bin Sub (Val 1) (Val 2))
        , assertParseConstruct "1 * 2" $ Arithmetic (Bin Mul (Val 1) (Val 2))
        ]

testParseBexp :: TestTree
testParseBexp =
    testGroup
        "Boolean Expression"
        [ assertParseConstruct "(true)" $ Boolean (Lit True)
        , assertParseConstruct "x < 0" $ Boolean (Rel Lt (Var "x") (Val 0))
        ]

testParseConstruct :: TestTree
testParseConstruct =
    testGroup
        "Construct"
        [ assertParseConstruct "/**/" Whitespace
        ]

testExecStm :: TestTree
testExecStm =
    testGroup
        "Statement"
        [ assertExec initial Skip ([], [])
        , assertExec initial (VarDef "x" Def (Val 10)) ([("x", 10)], [])
        , assertExec
            initial
            (IfElse (Lit True) (VarDef "x" Def (Val 1)) Skip)
            ([("x", 1)], [])
        , assertExec
            initial
            (IfElse (Lit False) Skip (VarDef "x" Def (Val 2)))
            ([("x", 2)], [])
        , let stm =
                While
                    (Rel Gt (Var "x") (Val 0))
                    (VarDef "x" Def (Bin Sub (Var "x") (Val 1)))
          in assertExec (setVar initial "x" 3) stm ([("x", 0)], [])
        , assertExec
            initial
            (Local "x" (Val 5) (VarDef "y" Def (Var "x")))
            ([("x", 0), ("y", 5)], []) -- x is unchanged
        , assertExec
            initial
            (Par (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2)))
            ([("x", 1), ("y", 2)], [])
        , assertExec
            initial
            (NonDet (VarDef "x" Def (Val 1)) (VarDef "x" Def (Val 2)))
            ([("x", 1), ("x", 2)], [])
        , let
            body = VarDef "x" Def (Bin Add (Var "x") (Val 1))
            stm =
                Seq
                    (ProcDef "inc" ["x"] ["x"] body)
                    (ProcInvoc "inc" [Val 10] ["y"])
          in
            assertExec initial stm ([("y", 11)], [("inc", (["x"], ["x"], body))])
        , let stm =
                VarDef "x" Def $
                    Time $
                        Seq
                            (Seq (VarDef "a" Def (Val 1)) (VarDef "a" Def (Val 2)))
                            (IfElse (Lit True) (VarDef "c" Def (Val 3)) Skip)
          in assertExec initial stm ([("x", 3)], [])
        , let stm =
                Seq
                    (VarDef "x" Def (Val 1))
                    (While (Not (Lit True)) (VarDef "x" Def (Val 1)))
          in assertExec initial stm ([("x", 1)], [])
        , assertExec (setVar initial "x" 0) (Revert (VarDef "x" Def (Val 1)) (Lit True)) ([("x", 0)], [])
        ]

testExecAexp :: TestTree
testExecAexp =
    testGroup
        "Arithmetic Expression"
        [ assertEvalAexp initial (Val 1) 1
        , assertEvalAexp initial (Var "x") 0
        , assertEvalAexp initial (Bin Add (Val 1) (Val 2)) 3
        , assertEvalAexp initial (Bin Sub (Val 2) (Val 3)) (-1)
        , assertEvalAexp initial (Bin Mul (Val 3) (Val 4)) 12
        , let stm =
                Time $
                    Seq
                        (Seq (VarDef "a" Def (Val 1)) (VarDef "a" Def (Val 2)))
                        (IfElse (Lit True) (VarDef "c" Def (Val 3)) Skip)
          in assertEvalAexp initial stm 3
        ]

testExecBexp :: TestTree
testExecBexp =
    testGroup
        "Boolean Expression"
        [ assertEvalBexp initial (Rel Eq (Val 1) (Val 1)) True
        , assertEvalBexp initial (Rel Neq (Val 1) (Val 2)) True
        , assertEvalBexp initial (Rel Lt (Val 1) (Val 2)) True
        , assertEvalBexp initial (Rel Leq (Val 1) (Val 2)) True
        , assertEvalBexp initial (Rel Gt (Val 2) (Val 1)) True
        , assertEvalBexp initial (Rel Geq (Val 2) (Val 1)) True
        , assertEvalBexp initial (Lit True) True
        , assertEvalBexp initial (Lit False) False
        , assertEvalBexp initial (And (Lit True) (Lit False)) False
        , assertEvalBexp initial (Or (Lit False) (Lit True)) True
        , assertEvalBexp initial (Not (Lit True)) False
        ]

assertParseStm :: String -> Stm -> TestTree
assertParseStm input expected =
    testCase input $
        parser "test" input @?= Right expected

assertParseConstruct :: String -> Construct -> TestTree
assertParseConstruct input expected =
    testCase input $
        parser "test" input @?= Right expected

assertEvalAexp :: State -> Aexp -> Integer -> TestTree
assertEvalAexp state e val = testCase (stringify e) $ evaluate e state @?= val

assertEvalBexp :: State -> Bexp -> Bool -> TestTree
assertEvalBexp state b bool = testCase (stringify b) $ evaluate b state @?= bool

assertExec :: State -> Stm -> ([(String, Integer)], [(String, Proc)]) -> TestTree
assertExec state stm (vars, procs) = testCase (stringify stm) $ do
    result <- runExceptT $ execute (stm, state)
    case result of
        Left err -> assertFailure $ "Execution failed with error: " ++ show err
        Right state' -> do
            case stm of
                NonDet _ _ -> do
                    let varChecks =
                            [ actual `elem` expected @? "NonDet: unexpected value for variable " ++ show k
                            | (k, _) <- vars
                            , let expected = [v' | (k', v') <- vars, k' == k]
                            , let actual = getVar state' k
                            ]
                    sequence_ varChecks
                    let procChecks =
                            [ actual `elem` map Just expected @? "NonDet: unexpected procedure " ++ show name
                            | (name, _) <- procs
                            , let expected = [p | (n, p) <- procs, n == name]
                            , let actual = getProc state' name
                            ]
                    sequence_ procChecks
                _ -> do
                    let varChecks = [getVar state' k @?= v | (k, v) <- vars]
                    sequence_ varChecks
                    let procChecks = [getProc state' name @?= Just p | (name, p) <- procs]
                    sequence_ procChecks

testParseCommand :: TestTree
testParseCommand =
    testGroup
        "Command"
        [ assertParseCommand "help" Help
        , assertParseCommand "?" Help
        , assertParseCommand "h" Help
        , assertParseCommand "quit" Quit
        , assertParseCommand "q" Quit
        , assertParseCommand "clear" Clear
        , assertParseCommand "c" Clear
        , assertParseCommand "version" Version
        , assertParseCommand "v" Version
        , assertParseCommand "reset" (Reset All)
        , assertParseCommand "reset vars" (Reset Vars)
        , assertParseCommand "reset procs" (Reset Procs)
        , assertParseCommand "reset break" (Reset Flag)
        , assertParseCommand "reset trace" (Reset Trace)
        , assertParseCommand "r" (Reset All)
        , assertParseCommand "show" (Show All)
        , assertParseCommand "show vars" (Show Vars)
        , assertParseCommand "show procs" (Show Procs)
        , assertParseCommand "show break" (Show Flag)
        , assertParseCommand "show trace" (Show Trace)
        , assertParseCommand "s" (Show All)
        , assertParseCommand "load foo.imp" (Load "foo.imp")
        , assertParseCommand "write out.imp" (Write "out.imp")
        , assertParseCommand "ast skip" (AST (Input (Statement Skip)))
        , assertParseCommand "ast #1" (AST (Index 1))
        , assertParseCommand "ast 1 + 2" (AST (Input (Arithmetic (Bin Add (Val 1) (Val 2)))))
        , assertParseCommand "set verbose normal" (Set (Verbose Normal))
        , assertParseCommand "set verbose profile" (Set (Verbose Profile))
        , assertParseCommand "set verbose debug" (Set (Verbose Debug))
        , assertParseCommand "set prompt IMP" (Set (Prompt "IMP"))
        , assertParseCommand "set prompt" (Set (Prompt ""))
        , assertParseCommand "set separator >" (Set (Separator '>'))
        ]

testExecCommand :: TestTree
testExecCommand =
    testGroup
        "Command"
        [ assertExecCommand "reset all" (reset All) $ \store ->
            _state store == initial && null (_trace store)
        , assertExecCommand "reset vars" (withVars >> reset Vars) $ \store ->
            getVars (_state store) == getVars initial
        , assertExecCommand "reset procs" (reset Procs) $ \store ->
            getProcs (_state store) == getProcs initial
        , assertExecCommand "reset trace" (withTrace >> reset Trace) $ \store ->
            null (_trace store)
        , assertExecCommand "set verbose debug" (set (Verbose Debug)) $ \store ->
            _verbose store == Debug
        , assertExecCommand "set verbose profile" (set (Verbose Profile)) $ \store ->
            _verbose store == Profile
        , assertExecCommand "set verbose normal" (set (Verbose Normal)) $ \store ->
            _verbose store == Normal
        , assertExecCommand "set prompt foo" (set (Prompt "foo")) $ \store ->
            _prompt store == "foo"
        , assertExecCommand "set separator +" (set (Separator '+')) $ \store ->
            _separator store == '+'
        , assertExecCommand "set goodbye bye" (set (Goodbye "bye")) $ \store ->
            _goodbye store == "bye"
        ]
    where
        withVars = modify $ \st -> st {_state = setVar (_state st) "x" 42}
        withTrace = modify $ \st -> st {_trace = [Skip]}

assertParseCommand :: String -> Command -> TestTree
assertParseCommand input expected =
    testCase input $
        parser "test" input @?= Right expected

silence :: IO a -> IO a
silence action =
    withFile "/dev/null" WriteMode $ \nullHandle ->
        bracket (hDuplicate stdout) hClose $ \stdoutDup -> do
            hDuplicateTo nullHandle stdout
            result <- action
            hDuplicateTo stdoutDup stdout
            return result

assertExecCommand :: String -> REPL IO () -> (Store -> Bool) -> TestTree
assertExecCommand name action check = testCase name $ do
    result <- silence $ runExceptT $ execStateT action start
    case result of
        Left e -> assertFailure $ "REPL action failed: " ++ show e
        Right store -> check store @? "Store assertion failed"
