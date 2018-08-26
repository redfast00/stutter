module Main where

import           Control.Monad    (forever, void)
import           System.Exit
import           System.IO

import           Parser           (parse)
import           StutterEvaluator
import           StutterParser
import           Types

main :: IO ()
main = readEvalPrintLoop

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    hSetBuffering stdout NoBuffering
    forever $ do putStr "stutter> "
                 line <- getLine
                 case line of
                     ":q" -> exitSuccess
                     x    -> eval x

eval :: String -> IO ()
eval input = do
    let result = parseStatement input
    either putStrLn succesparse result
    where succesparse x = do
            putStrLn "successful parse"
            extracted <- runTransformerStack undefined emptyEnvironment (evalStatement x)
            either putStrLn print extracted


parseStatement :: String -> Either String Expr
parseStatement a = case parse exprParse a of
    [(x,"")] -> Right x
    _        -> Left "parsing failed"
