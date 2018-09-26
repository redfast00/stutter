module Main where

import           Control.Monad      (foldM)
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

import           Builtins           (defaultEnvironmentStack)
import           Parser             (parseStatement)
import           StutterEvaluator   (evalStatement)
import           StutterParser      (fileParse, replLineParse)
import           Types


main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> readEvalPrintLoop defaultEnvironmentStack
        (path:_) -> runFile defaultEnvironmentStack path

readEvalPrintLoop :: EnvStack -> IO ()
readEvalPrintLoop environment = do
    hSetBuffering stdout NoBuffering
    repl environment

runFile :: EnvStack -> FilePath -> IO ()
runFile environment path = do
    content <- readFile path
    let parsed = parseStatement content fileParse
    case parsed of
        Left err -> putStrLn $ "ERROR in parse: " ++ err
        Right exprs -> do
            result <- foldM infold (Right (undefined, environment)) exprs
            case result of
                Left errormessage -> putStrLn $ "ERROR while evaluating: " ++ errormessage
                _ -> return ()

infold :: TransformerStackResult Expr -> Expr -> IO (TransformerStackResult Expr)
infold inp statement = case inp of
    a@(Left _)     -> return a
    Right (_, env) -> eval statement env

repl :: EnvStack -> IO ()
repl env = do
    putStr "stutter> "
    line <- getLine
    case line of
        ":q" -> exitSuccess
        _    -> do
            let parsed = parseStatement line replLineParse
            case parsed of
                Left err -> do
                    putStrLn $ "ERROR in parse: " ++ err
                    repl env
                Right expr -> do
                    result <- eval expr env
                    case result of
                        Left err -> do
                            putStrLn $ "ERROR in eval: " ++ err
                            repl env
                        Right (retval, newenv) -> do
                            print retval
                            repl newenv

-- | Evaluate single statement
eval :: Expr -> EnvStack -> IO (TransformerStackResult Expr)
eval statement env = runTransformerStack env (evalStatement statement)
