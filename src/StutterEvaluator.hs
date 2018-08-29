module StutterEvaluator (evalStatement) where

import           Types

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State

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
                    let finalEnvironment = defineVariable collector (StutterFexpr xs) environment
                    evalFunction finalEnvironment expression
                ("...":_) -> liftExcept $ throwError "incorrect vararg syntax"
                [] -> liftExcept $ throwError "no more arguments to apply"
                [variable] -> do
                    let finalEnvironment = defineVariable variable (head xs) environment
                    evalFunction finalEnvironment expression
                (variable:_) -> return $ StutterFunction (tail lambdaArgs, expression, defineVariable variable (head xs) environment)
            _ -> liftExcept $ throwError "s-expr should start with function"
evalStatement (StutterSymbol symbol) = lookupEnvironment symbol
evalStatement a = return a

evalFunction finalEnvironment expression = do
    pushEnvironment finalEnvironment
    retval <- evalStatement (StutterSexpr expression)
    _ <- popEnvironment
    return retval
