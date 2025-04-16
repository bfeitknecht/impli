{-# LANGUAGE OverloadedStrings #-}

module IMP.REPL (repl) where

import System.Console.Haskeline
import IMP.Parser (parseIMP)
import IMP.Exec (execStm)
import IMP.Eval (State)


-- IMP REPL
repl :: State -> IO ()
repl st = runInputT defaultSettings (loop st)
    where
        loop state = do
            input <- getInputLine "IMP> "
            case input of
                Nothing -> outputStrLn "Goodbye!"  -- ctrl-D
                Just ":quit" -> outputStrLn "Goodbye!"
                Just ":q" -> outputStrLn "Goodbye!"
                Just line -> case parseIMP "<interactive>" line of
                    Left err -> do
                        outputStrLn $ "Parse error: " ++ show err
                        loop state
                    Right stm -> do
                        let (state', output) = execStm stm state []
                        mapM_ outputStrLn output
                        loop state'
