{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Main
Description : Test suite for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Specification of parsing, pretty-printing and both semantics, including
agreement of natural and structural semantics on generated programs.
-}
module Main where

import Control.Monad.Except
import Control.Monad.State.Strict hiding (State)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.QuickCheck
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

import qualified Data.Map.Strict as Map

import IMP.Exception
import IMP.Expression
import IMP.Interpret
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Syntax
import REPL.Core (program)
import REPL.Input (incomplete)

import qualified IMP.Effect as Effect

-- | Scripted interpretation: inputs and choices are consumed from lists, output is collected.
newtype Script a = Script (StateT ([Integer], [Bool], [String]) (Either Exception) a)
    deriving (Applicative, Functor, Monad, MonadError Exception, MonadState ([Integer], [Bool], [String]))

instance Effect.MonadIMP Script where
    input _ = state $ \(is, cs, os) -> case is of
        i : is' -> (i, (is', cs, os))
        [] -> (0, ([], cs, os))
    output o = modify $ \(is, cs, os) -> (is, cs, o : os)
    choose = state $ \(is, cs, os) -> case cs of
        c : cs' -> (c, (is, cs', os))
        [] -> (True, (is, [], os))
    arbitrary = Effect.input "havoc"

-- | Run script with inputs, return result and output.
script :: [Integer] -> Script a -> Either Exception (a, [String])
script inputs (Script m) = fmap (\(a, (_, _, os)) -> (a, reverse os)) (runStateT m (inputs, [], []))

-- | Parse statement or fail test.
stm :: String -> Stm
stm source = either (error . show) id (parser "test" source)

-- | Execute source with semantics, return final state and output.
exec :: Semantics -> String -> Either Exception (State, [String])
exec sem source = script [] (execute sem (stm source) initial)

-- | Both semantics.
both :: [Semantics]
both = [Natural, Structural]

-- | Assert output of source under both semantics.
outputs :: String -> [String] -> TestTree
outputs source out =
    testCase source $
        mapM_
            (\sem -> fmap snd (exec sem source) @?= Right out)
            both

-- | Assert failure of source under both semantics.
fails :: String -> Exception -> TestTree
fails source e =
    testCase source $
        mapM_
            (\sem -> either Left (const (Right ())) (exec sem source) @?= Left e)
            both

main :: IO ()
main = do
    examples <- loadExamples
    defaultMain . localOption (QuickCheckTests 500) $
        testGroup
            "impli"
            [ parsing
            , semantics
            , regressions
            , properties
            , testGroup "Examples" examples
            ]

parsing :: TestTree
parsing =
    testGroup
        "Parsing"
        [ testCase "precedence of statements" $
            stm "a := 1; b := 2 par c := 3 [] d := 4"
                @?= Seq
                    (VarDef "a" Def (Val 1))
                    (Par (VarDef "b" Def (Val 2)) (NonDet (VarDef "c" Def (Val 3)) (VarDef "d" Def (Val 4))))
        , testCase "sequence is right associative" $
            stm "a := 1; b := 2; c := 3"
                @?= Seq (VarDef "a" Def (Val 1)) (Seq (VarDef "b" Def (Val 2)) (VarDef "c" Def (Val 3)))
        , testCase "parenthesized arithmetic in relation" $
            parser @Bexp "test" "(1 + 2) * 3 < x" @?= Right (Rel Lt (Bin Mul (Bin Add (Val 1) (Val 2)) (Val 3)) (Var "x"))
        , testCase "time extends over sequence" $
            stm "x := time a := 1; b := 2"
                @?= VarDef "x" Def (Time (Seq (VarDef "a" Def (Val 1)) (VarDef "b" Def (Val 2))))
        , testCase "boolean operators are reserved" $
            assertBool "not as variable" (either (const True) (const False) (parser @Stm "test" "not := 1"))
        , testCase "do until desugars to breaking loop" $
            stm "do x += 1 until x > 2 end"
                @?= While (Lit True) (Seq (VarDef "x" Inc (Val 1)) (IfElse (Rel Gt (Var "x") (Val 2)) Break Skip))
        , testCase "incomplete input continues" $ do
            incomplete "while x < 3 do" @?= True
            incomplete "while x < 3 do x += 1 end" @?= False
            incomplete "x := (1 +" @?= True
            incomplete "x := 1 )" @?= False
            incomplete ":show" @?= False
            incomplete "/* comment" @?= True
        ]

semantics :: TestTree
semantics =
    testGroup
        "Semantics"
        [ outputs "x := 3; while x > 0 do print x; x -= 1 end" ["3", "2", "1"]
        , outputs "x := 1; let x := 2 in print x end; print x" ["2", "1"]
        , outputs "let y := 2 in skip end; print y" ["0"]
        , outputs "procedure f(a; b) begin b := a * a end; f(7; r); print r" ["49"]
        , outputs
            "procedure fac(n; r) begin if n <= 1 then r := 1 else fac(n - 1; r); r *= n end end; fac(25; r); print r"
            ["15511210043330985984000000"]
        , outputs "x := 5; revert x := 1 if x = 1 end; print x" ["5"]
        , outputs "x := 5; revert x := 2 if x = 1 end; print x" ["2"]
        , outputs "try x := 1; raise 7; x := 2 catch e with print e end; print x" ["7", "0"]
        , outputs "match 2 on 1: print 1, 2: print 2, default: print 0 end" ["2"]
        , outputs "x := 1; y := 2; swap x y; print x; print y" ["2", "1"]
        , outputs "i := 0; while i < 4 do flip(0) print 1 flop print 2 end; i += 1 end" ["1", "2", "1", "2"]
        , outputs "repeat 3 times print 9 end" ["9", "9", "9"]
        , outputs "for i := 0 to 3 do print i end" ["0", "1", "2"]
        , outputs "x := 0; timeout while true do x += 1 end after 10 end; print x" ["3"]
        , outputs "x := 0; (x := x + 1; x := x + 1) alternate x := x * 10; print x" ["11"]
        , outputs "print (time (a := 1; b := 2))" ["3"]
        , outputs "print 7 / 0; print 7 % 0" ["0", "7"]
        , fails "assert 1 = 2" (AssertFail "1 = 2")
        , fails "raise 3" (Raised 3)
        , fails "f(; )" (Error "undefined procedure: f")
        , fails "print (time while true do skip end)" (Error "time exceeded limit of 1000000 steps")
        , testCase "read input" $
            fmap snd (script [42] (execute Natural (stm "read x; print x + 1") initial)) @?= Right ["43"]
        ]

regressions :: TestTree
regressions =
    testGroup
        "Regressions"
        [ outputs "x := 0; while true do break; x := 1 end; print x" ["0"]
        , outputs "i := 0; while true do let y := 0 in break end; i := 1 end; print i" ["0"]
        , outputs "i := 0; while i < 1 do i := 1; break end; j := 0; while j < 3 do j += 1 end; print j" ["3"]
        , outputs "x := 2; y := (time while x > 0 do x -= 1 end); print y; print x" ["7", "2"]
        , outputs "x := 0; do x += 1; if x = 5 then break end until x > 9 end; print x" ["5"]
        , outputs "while true do try break catch e with skip end end; print 1" ["1"]
        , outputs "while true do revert break if false end end; print 1" ["1"]
        , outputs "procedure p(; ) begin while true do break end end; p(; ); print 1" ["1"]
        , fails "break" (Error "break outside loop")
        , fails "procedure p(; ) begin break end" (Error "break outside loop")
        , testCase "pretty-print parenthesizes" $
            stringify (stm "x := (1 + 2) * 3") @?= "x := (1 + 2) * 3"
        ]

properties :: TestTree
properties =
    testGroup
        "Properties"
        [ testProperty "parse after pretty-print is identity" $ \(Program s) ->
            counterexample (prettify s) $ parser @Stm "property" (prettify s) === Right s
        , testProperty "natural and structural agree" $ \(Program s) ->
            counterexample (prettify s) $
                let run sem = either (Left . show) (Right . vars . fst) (script [] (execute sem s initial))
                in run Natural === run Structural
        ]

-- | Generated statement.
newtype Program = Program Stm
    deriving (Show)

instance Arbitrary Program where
    arbitrary = Program <$> sized (genStm True False)
    shrink (Program s) = map Program (shrinkStm s)

-- | Variables of generated programs.
names :: [String]
names = ["x", "y", "z"]

genAexp :: Int -> Gen Aexp
genAexp n
    | n <= 0 = oneof [Val <$> choose (-3, 9), Var <$> elements names]
    | otherwise =
        frequency
            [ (3, genAexp 0)
            , (3, Bin <$> elements [Add, Sub, Mul, Div, Mod] <*> genAexp (n `div` 2) <*> genAexp (n `div` 2))
            , (1, Time <$> genStm False False (n `div` 3))
            ]

genBexp :: Int -> Gen Bexp
genBexp n
    | n <= 0 = oneof [Lit <$> arbitrary, Rel <$> elements [Eq, Neq, Lt, Leq, Gt, Geq] <*> genAexp 1 <*> genAexp 1]
    | otherwise =
        oneof
            [ genBexp 0
            , Not <$> genBexp (n - 1)
            , And <$> genBexp (n `div` 2) <*> genBexp (n `div` 2)
            , Or <$> genBexp (n `div` 2) <*> genBexp (n `div` 2)
            ]

-- | Generate statement, whether procedures may be called and whether inside loop.
-- Loops are bounded by a guard counter and procedures don't call procedures.
genStm :: Bool -> Bool -> Int -> Gen Stm
genStm calls looping n
    | n <= 1 =
        oneof $
            [ return Skip
            , VarDef <$> elements names <*> elements [Def, Inc, Dec, Prod, Quot, Rem] <*> genAexp 2
            , Print <$> genAexp 1
            , Swap <$> elements names <*> elements names
            ]
                ++ [return Break | looping]
    | otherwise =
        let sub = genStm calls looping (n `div` 2)
        in frequency $
            [ (2, genStm calls looping 0)
            , (4, Seq <$> sub <*> sub)
            , (2, IfElse <$> genBexp 2 <*> sub <*> sub)
            , (2, bounded <$> elements ["c", "d"] <*> choose (0, 3) <*> genStm calls True (n `div` 2))
            , (1, Local <$> elements names <*> genAexp 1 <*> sub)
            , (1, Revert <$> sub <*> genBexp 1)
            , (1, Match <$> genAexp 1 <*> listOf ((,) <$> choose (0, 3) <*> sub) <*> sub)
            , (1, FlipFlop <$> choose (0, 2) <*> sub <*> sub)
            , (1, TryCatch <$> sub <*> elements names <*> sub)
            , (1, Raise <$> genAexp 1)
            , (1, Timeout <$> sub <*> genAexp 1)
            , (1, Alternate <$> sub <*> sub)
            ]
                ++ [(1, ProcDef "p" ["x"] ["y"] <$> genStm False False (n `div` 2)) | calls]
                ++ [(1, ProcInvoc "p" <$> vectorOf 1 (genAexp 1) <*> vectorOf 1 (elements names)) | calls]
    where
        bounded c k body = Local c (Val 0) (While (Rel Lt (Var c) (Val k)) (Seq (VarDef c Inc (Val 1)) body))

-- | Shrink statement to substatements.
shrinkStm :: Stm -> [Stm]
shrinkStm s = case s of
    Seq s1 s2 -> [s1, s2] ++ [Seq s1' s2 | s1' <- shrinkStm s1] ++ [Seq s1 s2' | s2' <- shrinkStm s2]
    IfElse b s1 s2 -> [s1, s2] ++ [IfElse b s1' s2 | s1' <- shrinkStm s1] ++ [IfElse b s1 s2' | s2' <- shrinkStm s2]
    Local x a s' -> s' : [Local x a s'' | s'' <- shrinkStm s']
    Revert s' b -> s' : [Revert s'' b | s'' <- shrinkStm s']
    TryCatch s1 _ s2 -> [s1, s2]
    Timeout s' a -> [s']
    Alternate s1 s2 -> [s1, s2]
    FlipFlop _ s1 s2 -> [s1, s2]
    Match _ ms d -> d : map snd ms
    _ -> []

-- | Expected outputs of deterministic examples, run with both semantics.
expected :: [(FilePath, [String])]
expected =
    [ ("countdown.imp", map show [10 :: Integer, 9 .. 1])
    , ("gauss.imp", ["4950"])
    , ("local.imp", ["59", "42"])
    , ("factorial.imp", map (show . (\n -> product [1 .. n])) [1 .. 20 :: Integer])
    , ("divmod.imp", [])
    ]

-- | Test every example runs, and deterministic ones produce expected output.
loadExamples :: IO [TestTree]
loadExamples = do
    let dir = "docs/examples"
    files <- filter ((== ".imp") . takeExtension) <$> listDirectory dir
    sources <- mapM (\f -> (,) f <$> readFile (dir </> f)) files
    return
        [ testCase (f ++ " " ++ show sem) $ case script [5, 3] (program sem f source) of
            Left e -> assertFailure (show e)
            Right (Left e, _) -> assertFailure (show e)
            Right (Right _, out) -> maybe (return ()) (out @?=) (lookup f expected)
        | (f, source) <- sources
        , sem <- both
        ]
