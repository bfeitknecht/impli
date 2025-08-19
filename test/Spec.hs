module Main (main) where

import Control.Monad.Trans.Except (runExceptT)
import Test.Tasty
import Test.Tasty.HUnit

import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
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
        , assertParseStm "var x := 1 in skip end" (Local "x" (Val 1) Skip)
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
            (Seq (ProcDef $ Procedure "foo" ([], []) Skip) (ProcInvoc "foo" ([], [])))
        , assertParseStm
            "repeat x := 1 until true"
            (Seq (VarDef "x" Def (Val 1)) (While (Not (Lit True)) (VarDef "x" Def (Val 1))))
        , assertParseStm
            "for i := 0 to 3 do skip end"
            (Local "i" (Val 0) (While (Rel Lt (Var "i") (Val 3)) (Seq Skip (VarDef "i" Inc (Val 1)))))
        , assertParseStm
            "do 4 times skip"
            (Local "_times" (Val 0) (While (Rel Lt (Var "_times") (Val 4)) (Seq Skip (VarDef "_times" Inc (Val 1)))))
        , assertParseStm
            "revert x := 1 if true"
            (Revert (VarDef "x" Def (Val 1)) (Lit True))
        , assertParseConstruct "(1)" $ Arithmetic (Val 1)
        , assertParseConstruct "x" $ Arithmetic (Var "x")
        , assertParseConstruct "1 + 2" $ Arithmetic (Bin Add (Val 1) (Val 2))
        , assertParseConstruct "1 - 2" $ Arithmetic (Bin Sub (Val 1) (Val 2))
        , assertParseConstruct "1 * 2" $ Arithmetic (Bin Mul (Val 1) (Val 2))
        , assertParseConstruct "(true)" $ Boolean (Lit True)
        , assertParseConstruct "x < 0" $ Boolean (Rel Lt (Var "x") (Val 0))
        , assertParseConstruct "/**/" Whitespace
        ]

evalTests :: TestTree
evalTests =
    testGroup
        "Expression Evaluation"
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
        , assertEvalBexp initial (Rel Eq (Val 1) (Val 1)) True
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

--
execTests :: TestTree
execTests =
    testGroup
        "Statement Execution"
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
                    (ProcDef $ Procedure "inc" (["x"], ["x"]) body)
                    (ProcInvoc "inc" ([Val 10], ["y"]))
          in
            assertExec initial stm ([("y", 11)], [Procedure "inc" (["x"], ["x"]) body])
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

precedenceTests :: TestTree
precedenceTests =
    testGroup
        "Operator Precedence"
        [ assertParseStm "x := 1 + 2 * 3" (VarDef "x" Def (Bin Add (Val 1) (Bin Mul (Val 2) (Val 3))))
        , assertParseStm "x := (1 + 2) * 3" (VarDef "x" Def (Bin Mul (Bin Add (Val 1) (Val 2)) (Val 3)))
        , assertParseStm
            "x := 1 [] y := 2 par z := 3"
            (Par (NonDet (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2))) (VarDef "z" Def (Val 3)))
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
    result <- runExceptT $ interpret (stm, state)
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
