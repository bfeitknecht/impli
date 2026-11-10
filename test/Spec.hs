{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import IMP.Engine
import IMP.Exception (Exception (..))
import IMP.Expression
import IMP.Kernel
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Syntax

import Frontend.Native.Meta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "IMP Tests"
        [ testParse
        , testKernel
        , testEngine
        , testMeta
        , testEval
        ]

-- | Handlers over IO with deterministic scripted resolutions: input and havoc yield 42, choice goes left.
scriptHandlers :: Handlers IO
scriptHandlers =
    Handlers
        { onInput = \_ -> return 42
        , onOutput = \_ -> return ()
        , onChoice = return True
        , onRandom = return 42
        , pollInterrupt = return False
        }

testParse :: TestTree
testParse =
    testGroup
        "Parse"
        [ testParseStm
        , testParseAexp
        , testParseBexp
        , testParseConstruct
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
        , assertParseStm "while true do skip end" (While (Lit True) Skip)
        , assertParseStm "print 1" (Print (Val 1))
        , assertParseStm "read x" (Read "x")
        , assertParseStm "let x := 1 in skip end" (Local "x" (Val 1) Skip)
        , assertParseStm
            "x := 1 par y := 2"
            (Par (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2)))
        , assertParseStm
            "x := 1 [] y := 2"
            (NonDet (VarDef "x" Def (Val 1)) (VarDef "x" Def (Val 2)))
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
            "revert x := 1 if true end"
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

testKernel :: TestTree
testKernel =
    testGroup
        "Kernel"
        [ testCase "step vardef updates residual state" $
            case step (VarDef "x" Def (Val 2), initial) of
                Stepped Step {residual = (Skip, state')} -> getVar state' "x" @?= 2
                conf -> assertFailure $ "expected stepped vardef, got: " ++ show conf
        , testCase "read suspends and resumes with input value" $
            case step (Read "x", initial) of
                Blocked (AwaitInput "x") ->
                    case resume (Read "x", initial) (ResolveInput 7) of
                        Stepped Step {residual = (Skip, state')} -> getVar state' "x" @?= 7
                        conf -> assertFailure $ "expected resumed read, got: " ++ show conf
                conf -> assertFailure $ "expected blocked read, got: " ++ show conf
        , testCase "havoc suspends and resumes with arbitrary value" $
            case step (Havoc "x", initial) of
                Blocked (AwaitHavoc "x") ->
                    case resume (Havoc "x", initial) (ResolveHavoc 7) of
                        Stepped Step {residual = (Skip, state')} -> getVar state' "x" @?= 7
                        conf -> assertFailure $ "expected resumed havoc, got: " ++ show conf
                conf -> assertFailure $ "expected blocked havoc, got: " ++ show conf
        , testCase "nondet suspends and resumes with branch choice" $
            case step (NonDet (VarDef "x" Def (Val 1)) (VarDef "x" Def (Val 2)), initial) of
                Blocked AwaitChoice ->
                    case resume (NonDet (VarDef "x" Def (Val 1)) (VarDef "x" Def (Val 2)), initial) (ResolveChoice False) of
                        Stepped Step {residual = (VarDef "x" Def (Val 2), _)} -> return ()
                        conf -> assertFailure $ "expected resumed right branch, got: " ++ show conf
                conf -> assertFailure $ "expected blocked choice, got: " ++ show conf
        , testCase "parallel suspends and resumes with order choice" $
            case step (Par (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2)), initial) of
                Blocked AwaitChoice ->
                    case resume (Par (VarDef "x" Def (Val 1)) (VarDef "y" Def (Val 2)), initial) (ResolveChoice False) of
                        Stepped Step {residual = (Seq (VarDef "y" Def (Val 2)) (VarDef "x" Def (Val 1)), _)} -> return ()
                        conf -> assertFailure $ "expected resumed reorder, got: " ++ show conf
                conf -> assertFailure $ "expected blocked choice, got: " ++ show conf
        , testCase "sequential read suspension resumes into remaining sequence" $
            case step (Seq (Read "x") (VarDef "y" Def (Val 1)), initial) of
                Blocked (AwaitInput "x") ->
                    case resume (Seq (Read "x") (VarDef "y" Def (Val 1)), initial) (ResolveInput 3) of
                        Stepped Step {residual = (Seq Skip (VarDef "y" Def (Val 1)), state')} ->
                            getVar state' "x" @?= 3
                        conf -> assertFailure $ "expected resumed sequence, got: " ++ show conf
                conf -> assertFailure $ "expected blocked sequence, got: " ++ show conf
        , testCase "trycatch suspension resumes through contexts" $ do
            let stm = TryCatch (Seq (Read "x") (Raise (Val 1))) "y" (VarDef "z" Def (Var "y"))
            (blocked, conf1) <- case step (stm, initial) of
                Blocked susp -> return (susp, resume (stm, initial) (ResolveInput 5))
                conf -> assertFailure $ "expected blocked trycatch, got: " ++ show conf
            case blocked of
                AwaitInput "x" -> return ()
                susp -> assertFailure $ "expected input suspension, got: " ++ show susp
            case conf1 of
                Stepped Step {residual = (TryCatch (Seq Skip (Raise (Val 1))) "y" _, state')} ->
                    getVar state' "x" @?= 5
                conf -> assertFailure $ "expected resumed wrapped trycatch, got: " ++ show conf
        , testCase "trycatch catches raise from inner statement" $
            case step (TryCatch (Raise (Val 1)) "y" Skip, initial) of
                Stepped Step {residual = (Skip, state')} -> getVar state' "y" @?= 1
                conf -> assertFailure $ "expected caught raise, got: " ++ show conf
        , testCase "invalid resolution is stuck" $
            case resume (Read "x", initial) (ResolveChoice True) of
                Stuck (Error _) -> return ()
                conf -> assertFailure $ "expected stuck on mismatch, got: " ++ show conf
        , testCase "procedure invocation rewrites to body with return snapshot" $ do
            let body = VarDef "x" Def (Bin Add (Var "x") (Val 1))
            let stm = ProcInvoc "inc" [Val 10] ["y"]
            let state = setProc initial "inc" (["x"], ["x"], body)
            case step (stm, state) of
                Stepped Step {residual = (Seq s' (Return {}), local)} -> do
                    s' @?= body
                    getVar local "x" @?= 10
                    getVar local "y" @?= 0 -- callee state is local
                conf -> assertFailure $ "expected invocation rewrite, got: " ++ show conf
        , testCase "local restores scope through restore snapshot" $
            case step (Local "x" (Val 5) (VarDef "y" Def (Var "x")), initial) of
                Stepped Step {residual = (Seq s' (Restore _), local)} -> do
                    s' @?= VarDef "y" Def (Var "x")
                    getVar local "x" @?= 5
                conf -> assertFailure $ "expected local rewrite, got: " ++ show conf
        , testCase "timeout expires to skip" $
            case step (Timeout Skip (Val 0), initial) of
                Stepped Step {residual = (Skip, _)} -> return ()
                conf -> assertFailure $ "expected timeout expiry, got: " ++ show conf
        , testCase "alternate interleaves" $
            case step (Alternate (VarDef "x" Def (Val 1)) Skip, initial) of
                Stepped Step {residual = (Alternate s' _, state')} -> do
                    s' @?= Skip
                    getVar state' "x" @?= 1
                conf -> assertFailure $ "expected alternate interleave, got: " ++ show conf
        ]

testEngine :: TestTree
testEngine =
    testGroup
        "Engine"
        [ testCase "drive natural commits trace and environment" $ do
            let stm = VarDef "x" Def (Val 1)
            outcome <- runEngine scriptHandlers boot (drive Natural stm)
            case outcome of
                Left fault -> assertFailure $ "drive failed: " ++ show fault
                Right (_, session) -> do
                    getVar (environment session) "x" @?= 1
                    trace session @?= [stm]
        , testCase "drive natural soaks up read via handler" $ do
            outcome <- runEngine scriptHandlers boot (drive Natural (Read "x"))
            case outcome of
                Left fault -> assertFailure $ "drive failed: " ++ show fault
                Right (_, session) -> getVar (environment session) "x" @?= 42
        , testCase "drive structural suspends on read and handler resolves" $ do
            outcome <- runEngine scriptHandlers boot (drive Structural (Read "x"))
            case outcome of
                Left fault -> assertFailure $ "drive failed: " ++ show fault
                Right (conf, session) -> do
                    case conf of
                        Terminal state' -> getVar state' "x" @?= 42
                        other -> assertFailure $ "expected terminal, got: " ++ show other
                    getVar (environment session) "x" @?= 42
                    trace session @?= [Read "x"]
        , testCase "begin blocks and proceed resolves" $ do
            outcome <- runEngine scriptHandlers boot $ do
                conf <- begin (Read "x")
                case conf of
                    Blocked _ -> proceed (ResolveInput 9)
                    other -> return other
            case outcome of
                Left fault -> assertFailure $ "engine failed: " ++ show fault
                Right (conf, session) -> case conf of
                    Terminal state' -> getVar state' "x" @?= 9
                    other -> assertFailure $ "expected terminal, got: " ++ show other
        , testCase "begin on busy flight faults" $ do
            outcome <- runEngine scriptHandlers boot $ do
                _ <- begin (Read "x")
                _ <- begin Skip
                return ()
            case outcome of
                Left BusyFlight -> return ()
                other -> assertFailure $ "expected busy fault, got: " ++ show other
        , testCase "proceed without flight faults" $ do
            outcome <- runEngine scriptHandlers boot (proceed (ResolveInput 1))
            case outcome of
                Left NoFlight -> return ()
                other -> assertFailure $ "expected no-flight fault, got: " ++ show other
        , testCase "stuck statement clears flight" $ do
            outcome <- runEngine scriptHandlers boot (drive Structural (Assert (Lit False)))
            case outcome of
                Left (RuntimeFault (AssertFail _)) -> return ()
                other -> assertFailure $ "expected assert failure, got: " ++ show other
        , testCase "reset aspects" $ do
            outcome <- runEngine scriptHandlers boot $ do
                _ <- drive Natural (VarDef "x" Def (Val 1))
                resetAspect All
            case outcome of
                Left fault -> assertFailure $ "reset failed: " ++ show fault
                Right (_, session) -> do
                    environment session @?= initial
                    trace session @?= []
        , testCase "procedure call by structural drive" $ do
            let body = VarDef "x" Def (Bin Add (Var "x") (Val 1))
            let stm = Seq (ProcDef "inc" ["x"] ["x"] body) (ProcInvoc "inc" [Val 10] ["y"])
            outcome <- runEngine scriptHandlers boot (drive Structural stm)
            case outcome of
                Left fault -> assertFailure $ "drive failed: " ++ show fault
                Right (_, session) -> do
                    getVar (environment session) "y" @?= 11
                    getVar (environment session) "x" @?= 0 -- callee scope does not leak
        , testCase "rule profile is recorded" $ do
            outcome <- runEngine scriptHandlers boot (drive Structural (VarDef "x" Def (Val 1)))
            case outcome of
                Left fault -> assertFailure $ "drive failed: " ++ show fault
                Right (_, session) -> stepCount (profile session) > 0 @? "expected recorded steps"
        ]

testMeta :: TestTree
testMeta =
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
        , assertParseCommand "set verbose normal" (Verbose Normal)
        , assertParseCommand "set verbose profile" (Verbose Profile)
        , assertParseCommand "set verbose debug" (Verbose Debug)
        ]

testEval :: TestTree
testEval =
    testGroup
        "Evaluate"
        [ testExecStm
        , testExecAexp
        , testExecBexp
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
        , assertExec
            (setVar initial "x" 3)
            (While (Rel Gt (Var "x") (Val 0)) (VarDef "x" Dec (Val 1)))
            ([("x", 0)], [])
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
            ([("x", 1)], []) -- scripted choice goes left
        , let
            body = VarDef "x" Def (Bin Add (Var "x") (Val 1))
            stm =
                Seq
                    (ProcDef "inc" ["x"] ["x"] body)
                    (ProcInvoc "inc" [Val 10] ["y"])
          in
            assertExec initial stm ([("y", 11)], [("inc", (["x"], ["x"], body))])
        , assertExec
            initial
            (Seq (Read "x") (VarDef "y" Def (Var "x")))
            ([("x", 42), ("y", 42)], []) -- scripted input is 42
        , assertExec
            initial
            (VarDef "x" Def (Time (Seq (VarDef "a" Def (Val 1)) (VarDef "b" Def (Val 2)))))
            ([("x", 2)], [])
        , assertExec (setVar initial "x" 0) (Revert (VarDef "x" Def (Val 1)) (Lit True)) ([("x", 0)], [])
        , assertExec initial (Havoc "x") ([("x", 42)], []) -- scripted havoc is 42
        , assertExec
            initial
            (TryCatch (Raise (Val 5)) "x" Skip)
            ([("x", 5)], [])
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
        , assertEvalBexp initial (Rel Geq (Val 1) (Val 1)) True
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

assertParseCommand :: String -> Command -> TestTree
assertParseCommand input expected =
    testCase input $
        parser "test" input @?= Right expected

assertEvalAexp :: State -> Aexp -> Integer -> TestTree
assertEvalAexp state e val = testCase (stringify e) $ evaluate e state @?= val

assertEvalBexp :: State -> Bexp -> Bool -> TestTree
assertEvalBexp state b bool = testCase (stringify b) $ evaluate b state @?= bool

-- | Assert execution result with both semantics, using scripted handlers.
assertExec :: State -> Stm -> ([(String, Integer)], [(String, Proc)]) -> TestTree
assertExec state stm (vars, procs) =
    testGroup (stringify stm) [assertWith Natural, assertWith Structural]
    where
        assertWith semantics = testCase (show semantics) $ do
            outcome <- runEngine scriptHandlers boot {environment = state} (drive semantics stm)
            case outcome of
                Left fault -> assertFailure $ "execution failed with fault: " ++ show fault
                Right (_, session) -> do
                    let env = environment session
                    sequence_ [getVar env k @?= v | (k, v) <- vars]
                    sequence_ [getProc env name @?= Just p | (name, p) <- procs]
