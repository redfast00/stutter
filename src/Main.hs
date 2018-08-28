module Main where

import           Control.Monad    (forever, void)
import           System.Exit
import           System.IO

import           Builtins
import           Parser           (parse)
import           StutterEvaluator
import           StutterParser
import           Types

main :: IO ()
main = readEvalPrintLoop

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    hSetBuffering stdout NoBuffering
    repl' defaultEnvironment
    return ()

repl' :: Environment -> IO ()
repl' env = do
    putStr "stutter> "
    line <- getLine
    case line of
        ":q" -> exitSuccess
        x    -> do
            newEnvironment <- eval x env
            case newEnvironment of
                Nothing -> repl' env
                (Just newenv) -> repl' newenv

eval :: String -> Environment -> IO (Maybe Environment)
eval input env = do
    let result = parseStatement input
    either failparse succesparse result
    where succesparse x = do
            -- print x -- print AST
            extracted <- runTransformerStack undefined env (evalStatement x)
            case extracted of
                (Left errorMessage) -> do
                    putStrLn errorMessage
                    return Nothing
                (Right (result, environment)) -> do
                    print result
                    return (Just environment)
          failparse x = do
               print x
               return Nothing

parseStatement :: String -> Either String Expr
parseStatement a = case parse exprParse a of
    [(x,"")] -> Right x
    _        -> Left "parsing failed"
