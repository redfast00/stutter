module Main where

import           Control.Monad      (foldM_)
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

import           Builtins
import           Parser             (parseStatement)
import           StutterEvaluator   (evalStatement)
import           StutterParser      (fileParse, replLineParse)
import           Types

import           MBot               (Device)

main :: IO ()
main = do
    args <- getArgs
    let device = undefined
    case args of
        []       -> readEvalPrintLoop defaultEnvironment device
        (path:_) -> runFile defaultEnvironment device path

readEvalPrintLoop :: Environment -> Device -> IO ()
readEvalPrintLoop environment device = do
    hSetBuffering stdout NoBuffering
    repl' environment device

runFile :: Environment -> Device -> FilePath -> IO ()
runFile environment device path = do
    content <- readFile path
    let parsed = parseStatement content fileParse
    case parsed of
        Left err -> putStrLn $ "ERROR in parse: " ++ err
        Right exprs -> foldM_ infold (device, Right (undefined, environment)) exprs

infold :: (Device, TransformerStackResult Expr) -> Expr -> IO (Device, TransformerStackResult Expr)
infold inp statement = case inp of
    a@(_, Left _)      -> return a
    (device, Right (_, env)) -> do
        q <- eval statement env device
        return (device, q)

repl' :: Environment -> Device -> IO ()
repl' env device = do
    putStr "stutter> "
    line <- getLine
    case line of
        ":q" -> exitSuccess
        _    -> do
            let parsed = parseStatement line replLineParse
            case parsed of
                Left err -> do
                    putStrLn $ "ERROR in parse: " ++ err
                    repl' env device
                Right expr -> do
                    result <- eval expr env device
                    case result of
                        Left err -> do
                            putStrLn $ "ERROR in eval: " ++ err
                            repl' env device
                        Right (retval, newenv) -> do
                            print retval
                            repl' newenv device

-- | Evaluate single statement
eval :: Expr -> Environment -> Device -> IO (TransformerStackResult Expr)
eval statement env device = runTransformerStack device env (evalStatement statement)
