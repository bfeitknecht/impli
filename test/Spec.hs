module Main (main) where

import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit

import IMP.Parser
import IMP.Pretty
import IMP.Semantics
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
        ]

-- add operator precedence parse
parseTests :: TestTree
parseTests =
    testGroup
        "Parse"
        [ assertParseStm "skip" Skip
        , assertParseStm "x := 1" (VarDef "x" (Numeral 1))
        , assertParseStm "if true then skip end" (If (Boolean True) Skip Skip)
        , assertParseStm "if true then skip else skip end" (If (Boolean True) Skip Skip)
        , assertParseStm "while true do skip end" (While (Boolean True) Skip)
        , assertParseStm "var x := 1 in skip end" (Local "x" (Numeral 1) Skip)
        , assertParseStm "x := 1; y := 2" (Seq (VarDef "x" (Numeral 1)) (VarDef "y" (Numeral 2)))
        , assertParseStm "x := 1 par y := 2" (Par (VarDef "x" (Numeral 1)) (VarDef "y" (Numeral 2)))
        , assertParseStm "skip par skip; skip par skip" (Seq (Par Skip Skip) (Par Skip Skip))
        , assertParseStm "x := 1 || y := 2" (NonDet (VarDef "x" (Numeral 1)) (VarDef "y" (Numeral 2)))
        , assertParseConstruct "1" $ Arithm (Numeral 1)
        , assertParseConstruct "x" $ Arithm (Variable "x")
        , assertParseConstruct "1 + 2" $ Arithm (Bin Add (Numeral 1) (Numeral 2))
        , assertParseConstruct "1 - 2" $ Arithm (Bin Sub (Numeral 1) (Numeral 2))
        , assertParseConstruct "1 * 2" $ Arithm (Bin Mul (Numeral 1) (Numeral 2))
        , assertParseConstruct "(true)" $ Bool (Boolean True)
        , assertParseConstruct "/**/" $ Whitespace
        ]

-- add operator precedence
evalTests :: TestTree
evalTests =
    testGroup
        "Expression Evaluation"
        [ assertEvalAexp initial (Numeral 1) 1
        , assertEvalAexp initial (Variable "x") 0
        , assertEvalAexp initial (Bin Add (Numeral 1) (Numeral 2)) 3
        , assertEvalAexp initial (Bin Sub (Numeral 4) (Numeral 5)) (-1)
        , assertEvalAexp initial (Bin Mul (Numeral 2) (Numeral 3)) 6
        , assertEvalBexp initial (Rel Eq (Numeral 1) (Numeral 1)) True
        , assertEvalBexp initial (Rel Neq (Numeral 1) (Numeral 2)) True
        , assertEvalBexp initial (Rel Lt (Numeral 1) (Numeral 2)) True
        , assertEvalBexp initial (Rel Leq (Numeral 1) (Numeral 2)) True
        , assertEvalBexp initial (Rel Gt (Numeral 2) (Numeral 1)) True
        , assertEvalBexp initial (Rel Geq (Numeral 2) (Numeral 1)) True
        , assertEvalBexp initial (Boolean True) True
        , assertEvalBexp initial (Boolean False) False
        ]

execTests :: TestTree
execTests =
    testGroup
        "Statement Execution"
        [ assertExec initial Skip ([], [])
        , assertExec initial (VarDef "x" (Numeral 10)) ([("x", 10)], [])
        , assertExec
            initial
            (If (Boolean True) (VarDef "x" (Numeral 1)) Skip)
            ([("x", 1)], [])
        , assertExec
            initial
            (If (Boolean False) Skip (VarDef "x" (Numeral 2)))
            ([("x", 2)], [])
        , let
            cond = Rel Gt (Variable "x") (Numeral 0)
            body = VarDef "x" (Bin Sub (Variable "x") (Numeral 1))
            state = setVar initial "x" 3
          in
            assertExec state (While cond body) ([("x", 0)], [])
        , assertExec
            initial
            (Local "x" (Numeral 5) (VarDef "y" (Variable "x")))
            ([("x", 0), ("y", 5)], []) -- x is unchanged
        , assertExec
            initial
            (NonDet (VarDef "x" (Numeral 1)) (VarDef "x" (Numeral 2)))
            ([("x", 1), ("x", 2)], [])
        , assertExec
            initial
            (Par (VarDef "x" (Numeral 1)) (VarDef "y" (Numeral 2)))
            ([("x", 1), ("y", 2)], [])
        , let
            body = VarDef "x" (Bin Add (Variable "x") (Numeral 1))
            def = ProcDef "inc" (["x"], ["x"]) body
            invoc = ProcInvoc "inc" ([Numeral 10], ["y"])
          in
            assertExec
                initial
                (Seq def invoc)
                ([("y", 11)], [("inc", Proc (["x"], ["x"]) body)])
        ]

assertParseStm :: String -> Stm -> TestTree
assertParseStm input expected =
    testCase input $
        parseProgram "test" input @?= Right expected

assertParseConstruct :: String -> Construct -> TestTree
assertParseConstruct input expected =
    testCase input $
        parseInput "test" input @?= Right expected

assertEvalAexp :: State -> Aexp -> Val -> TestTree
assertEvalAexp state e val = testCase (pretty e) $ evalAexp state e @?= val

assertEvalBexp :: State -> Bexp -> Bool -> TestTree
assertEvalBexp state b bool = testCase (pretty b) $ evalBexp state b @?= bool

assertExec :: State -> Stm -> ([(Var, Val)], [(Var, Proc)]) -> TestTree
assertExec state stm (vars, procs) = testCase (pretty stm) $ do
    state' <- execStm state stm
    case stm of
        NonDet _ _ -> do
            forM_ vars $ \(k, _) -> do
                let
                    expected = [v | (k', v) <- vars, k' == k]
                    actual = getVar state' k
                actual `elem` expected @? "NonDet: unexpected value for variable " ++ show k
            forM_ procs $ \(k, _) -> do
                let
                    expected = [p | (k', p) <- procs, k' == k]
                    actual = getProc state' k
                actual `elem` map Just expected @? "NonDet: unexpected procedure " ++ show k
        _ -> do
            forM_ vars $ \(k, v) ->
                getVar state' k @?= v
            forM_ procs $ \(k, p) ->
                getProc state' k @?= Just p
