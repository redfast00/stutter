module Main where

import           Control.Monad (forever)
import           System.Exit
import           System.IO

import Parser (parse)
import           StutterParser

main :: IO ()
main = readEvalPrintLoop

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    hSetBuffering stdout NoBuffering
    forever $ do putStr "stutter> "
                 line <- getLine
                 case line of
                     ":q" -> exitSuccess
                     x    -> do eval x
                                readEvalPrintLoop

eval :: String -> IO ()
eval input = print (parse exprParse input)
