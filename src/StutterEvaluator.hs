module StutterEvaluator (evalStatement) where

import           Types

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict  as Map

evalStatement :: Expr -> TransformerStack Expr
evalStatement s@(StutterSexpr arguments) = case arguments of
    []  -> return s
    [x] -> evalStatement x
    (x:xs) -> do
        function <- evalStatement x
        case function of
            (StutterBuiltin builtin) -> do
                evaluatedArgs <- mapM evalStatement xs
                builtin evaluatedArgs
            (StutterFunction (lambdaArgs, expression, environment)) -> case lambdaArgs of
                ["...", collector] -> do
                    let finalEnvironment = Map.insert collector (StutterFexpr xs) environment
                    pushEnvironment finalEnvironment
                    retval <- evalStatement (StutterSexpr expression)
                    _ <- popEnvironment
                    return retval
                ("...":_) -> liftExcept $ throwError "incorrect vararg syntax"
                [] -> liftExcept $ throwError "no more arguments to apply"
                [variable] -> do
                    let finalEnvironment = Map.insert variable (head xs) environment
                    pushEnvironment finalEnvironment
                    retval <- evalStatement (StutterSexpr expression)
                    _ <- popEnvironment
                    return retval
                (variable:_) -> return $ StutterFunction (tail lambdaArgs, expression, Map.insert variable (head xs) environment)
            _ -> liftExcept $ throwError "s-expr should start with function"
evalStatement (StutterSymbol symbol) = lookupEnvironment symbol
evalStatement a = return a
