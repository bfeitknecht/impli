module Main (main) where

import Control.Monad.Trans.Except (runExceptT)
import System.Console.Haskeline (defaultSettings, runInputT)
import Test.Tasty
import Test.Tasty.HUnit

import IMP.Parser
import IMP.Pretty
import IMP.Result
import IMP.Semantics.Expression
import IMP.Semantics.State
import IMP.Semantics.Statement
import IMP.Syntax

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "IMP Tests"
        [ parseTests
        , evalTests
        , execTests
        , precedenceTests
        ]

parseTests :: TestTree
parseTests =
    testGroup
        "Parse"
        [ assertParseStm "skip" Skip
        , assertParseStm "x := 1" (VarDef "x" Def (Numeral 1))
        , assertParseStm "(skip; skip)" (Seq Skip Skip)
        , assertParseStm "if true then skip end" (If (Boolean True) Skip Skip)
        , assertParseStm "if true then skip else skip end" (If (Boolean True) Skip Skip)
        , assertParseStm
            "if true then skip else if true then skip end end"
            (If (Boolean True) Skip (If (Boolean True) Skip Skip))
        , assertParseStm
            "while false do if true then skip else if true then skip end end end"
            (While (Boolean False) (If (Boolean True) Skip (If (Boolean True) Skip Skip)))
        , {-
          , assertParseStm
              "if true then skip else if true then skip end"
              (If (Boolean True) Skip (If (Boolean True) Skip Skip))
          , assertParseStm
              "while false do if true then skip else if true then skip end end"
              (While (Boolean False) (If (Boolean True) Skip (If (Boolean True) Skip Skip)))
          -}
          assertParseStm "while true do skip end" (While (Boolean True) Skip)
        , assertParseStm "print 1" (Print (Numeral 1))
        , assertParseStm "read x" (Read "x")
        , assertParseStm "var x := 1 in skip end" (Local "x" (Numeral 1) Skip)
        , assertParseStm
            "x := 1 par y := 2"
            (Par (VarDef "x" Def (Numeral 1)) (VarDef "y" Def (Numeral 2)))
        , assertParseStm
            "x := 1 [] y := 2"
            (NonDet (VarDef "x" Def (Numeral 1)) (VarDef "y" Def (Numeral 2)))
        , assertParseStm
            "skip par skip; skip [] skip"
            (Seq (Par Skip Skip) (NonDet Skip Skip))
        , assertParseStm
            "x := time (a := 1; b := 2)"
            (VarDef "x" Def (Time (Seq (VarDef "a" Def (Numeral 1)) (VarDef "b" Def (Numeral 2)))))
        , assertParseStm
            "procedure foo(;) begin skip end; foo(;)"
            (Seq (ProcDef $ Proc "foo" ([], []) Skip) (ProcInvoc "foo" ([], [])))
        , assertParseStm
            "repeat x := 1 until true"
            (Seq (VarDef "x" Def (Numeral 1)) (While (Not (Boolean True)) (VarDef "x" Def (Numeral 1))))
        , assertParseStm
            "for i := 0 to 3 do skip end"
            (Local "i" (Numeral 0) (While (Rel Lt (Variable "i") (Numeral 3)) (Seq Skip (VarDef "i" Inc (Numeral 1)))))
        , assertParseStm
            "do 4 times skip"
            (Local "_times" (Numeral 0) (While (Rel Lt (Variable "_times") (Numeral 4)) (Seq Skip (VarDef "_times" Inc (Numeral 1)))))
        , assertParseStm
            "revert x := 1 if true"
            (Revert (VarDef "x" Def (Numeral 1)) (Boolean True))
        , assertParseConstruct "(1)" $ Arithm (Numeral 1)
        , assertParseConstruct "x" $ Arithm (Variable "x")
        , assertParseConstruct "1 + 2" $ Arithm (Bin Add (Numeral 1) (Numeral 2))
        , assertParseConstruct "1 - 2" $ Arithm (Bin Sub (Numeral 1) (Numeral 2))
        , assertParseConstruct "1 * 2" $ Arithm (Bin Mul (Numeral 1) (Numeral 2))
        , assertParseConstruct "(true)" $ Bool (Boolean True)
        , assertParseConstruct "x < 0" $ Bool (Rel Lt (Variable "x") (Numeral 0))
        , assertParseConstruct "/**/" $ Whitespace
        ]

evalTests :: TestTree
evalTests =
    testGroup
        "Expression Evaluation"
        [ assertEvalAexp initial (Numeral 1) 1
        , assertEvalAexp initial (Variable "x") 0
        , assertEvalAexp initial (Bin Add (Numeral 1) (Numeral 2)) 3
        , assertEvalAexp initial (Bin Sub (Numeral 2) (Numeral 3)) (-1)
        , assertEvalAexp initial (Bin Mul (Numeral 3) (Numeral 4)) 12
        , let stm =
                Time $
                    Seq
                        (Seq (VarDef "a" Def (Numeral 1)) (VarDef "a" Def (Numeral 2)))
                        (If (Boolean True) (VarDef "c" Def (Numeral 3)) Skip)
          in assertEvalAexp initial stm 3
        , assertEvalBexp initial (Rel Eq (Numeral 1) (Numeral 1)) True
        , assertEvalBexp initial (Rel Neq (Numeral 1) (Numeral 2)) True
        , assertEvalBexp initial (Rel Lt (Numeral 1) (Numeral 2)) True
        , assertEvalBexp initial (Rel Leq (Numeral 1) (Numeral 2)) True
        , assertEvalBexp initial (Rel Gt (Numeral 2) (Numeral 1)) True
        , assertEvalBexp initial (Rel Geq (Numeral 2) (Numeral 1)) True
        , assertEvalBexp initial (Boolean True) True
        , assertEvalBexp initial (Boolean False) False
        , assertEvalBexp initial (And (Boolean True) (Boolean False)) False
        , assertEvalBexp initial (Or (Boolean False) (Boolean True)) True
        , assertEvalBexp initial (Not (Boolean True)) False
        ]

--
execTests :: TestTree
execTests =
    testGroup
        "Statement Execution"
        [ assertExec initial Skip ([], [])
        , assertExec initial (VarDef "x" Def (Numeral 10)) ([("x", 10)], [])
        , assertExec
            initial
            (If (Boolean True) (VarDef "x" Def (Numeral 1)) Skip)
            ([("x", 1)], [])
        , assertExec
            initial
            (If (Boolean False) Skip (VarDef "x" Def (Numeral 2)))
            ([("x", 2)], [])
        , let stm =
                While
                    (Rel Gt (Variable "x") (Numeral 0))
                    (VarDef "x" Def (Bin Sub (Variable "x") (Numeral 1)))
          in assertExec (setVar initial "x" 3) stm ([("x", 0)], [])
        , assertExec
            initial
            (Local "x" (Numeral 5) (VarDef "y" Def (Variable "x")))
            ([("x", 0), ("y", 5)], []) -- x is unchanged
        , assertExec
            initial
            (Par (VarDef "x" Def (Numeral 1)) (VarDef "y" Def (Numeral 2)))
            ([("x", 1), ("y", 2)], [])
        , assertExec
            initial
            (NonDet (VarDef "x" Def (Numeral 1)) (VarDef "x" Def (Numeral 2)))
            ([("x", 1), ("x", 2)], [])
        , let
            body = VarDef "x" Def (Bin Add (Variable "x") (Numeral 1))
            stm =
                Seq
                    (ProcDef $ Proc "inc" (["x"], ["x"]) body)
                    (ProcInvoc "inc" ([Numeral 10], ["y"]))
          in
            assertExec initial stm ([("y", 11)], [(Proc "inc" (["x"], ["x"]) body)])
        , let stm =
                VarDef "x" Def $
                    Time $
                        Seq
                            (Seq (VarDef "a" Def (Numeral 1)) (VarDef "a" Def (Numeral 2)))
                            (If (Boolean True) (VarDef "c" Def (Numeral 3)) Skip)
          in assertExec initial stm ([("x", 3)], [])
        , let stm =
                Seq
                    (VarDef "x" Def (Numeral 1))
                    (While (Not (Boolean True)) (VarDef "x" Def (Numeral 1)))
          in assertExec initial stm ([("x", 1)], [])
        , assertExec (setVar initial "x" 0) (Revert ((VarDef "x") Def (Numeral 1)) (Boolean True)) ([("x", 0)], [])
        ]

precedenceTests :: TestTree
precedenceTests =
    testGroup
        "Operator Precedence"
        [ assertParseStm "x := 1 + 2 * 3" (VarDef "x" Def (Bin Add (Numeral 1) (Bin Mul (Numeral 2) (Numeral 3))))
        , assertParseStm "x := (1 + 2) * 3" (VarDef "x" Def (Bin Mul (Bin Add (Numeral 1) (Numeral 2)) (Numeral 3)))
        , assertParseStm
            "x := 1 [] y := 2 par z := 3"
            (Par (NonDet (VarDef "x" Def (Numeral 1)) (VarDef "y" Def (Numeral 2))) (VarDef "z" Def (Numeral 3)))
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
assertEvalAexp state e val = testCase (stringify e) $ evaluate state e @?= val

assertEvalBexp :: State -> Bexp -> Bool -> TestTree
assertEvalBexp state b bool = testCase (stringify b) $ evaluate state b @?= bool

assertExec :: State -> Stm -> ([(String, Integer)], [Proc]) -> TestTree
assertExec state stm (vars, procs) = testCase (stringify stm) $ do
    result <- launch $ interpret state stm
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
                            [ actual `elem` map Just expected @? "NonDet: unexpected procedure " ++ show (procname p)
                            | p <- procs
                            , let expected = [p' | p' <- procs, procname p' == procname p]
                            , let actual = getProc state' (procname p)
                            ]
                    sequence_ procChecks
                _ -> do
                    let varChecks = [getVar state' k @?= v | (k, v) <- vars]
                    sequence_ varChecks
                    let procChecks = [getProc state' (procname proc) @?= Just proc | proc <- procs]
                    sequence_ procChecks

launch :: REPL a -> IO (Either Result a)
launch repl = runInputT defaultSettings (runExceptT repl)
