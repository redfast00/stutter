module Main where

import           Control.Exception  (tryJust)
import           Control.Monad      (forM_)
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           System.IO.Error    (ioeGetErrorType, isDoesNotExistErrorType)

import           Builtins           (defaultEnvironmentStack)
import           StutterEvaluator   (evalStatement)
import           StutterParser      (fileParse, parseThing, replLineParse)
import           Types


main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> readEvalPrintLoop defaultEnvironmentStack
        (path:_) -> do
            _ <- runTransformerStack defaultEnvironmentStack (preludeExec >> tryStack (runFile path) ())
            return ()

readEvalPrintLoop :: EnvStack -> IO ()
readEvalPrintLoop environment = do
    _ <- hSetBuffering stdout NoBuffering
    _ <- runTransformerStack environment (preludeExec >> keepOnDoing repl)
    return ()

keepOnDoing :: TransformerStack () -> TransformerStack ()
keepOnDoing action = do
    tryStack action ()
    keepOnDoing action

preludeExec :: TransformerStack ()
preludeExec = tryStack (runFile "prelude.stutter") ()

runFile ::  FilePath -> TransformerStack ()
runFile path = do
    -- Try to read file, catch exception if file can't be read
    eitherContent <- liftIO $ tryJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (readFile path)
    case eitherContent of
        Left _ -> throwStutterError $ "Could not open file " ++ path
        Right content -> do
            exprs <- parseThing fileParse content
            forM_ exprs evalStatement

repl :: TransformerStack ()
repl = do
    liftIO $ putStr "stutter> "
    line <- liftIO getLine
    case line of
        ":q" -> liftIO exitSuccess
        _    -> do
            expr <- parseThing replLineParse line
            r <- evalStatement expr
            liftIO $ print r
