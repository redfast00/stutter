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
                    let finalEnvironment = addToEnvironment collector (StutterFexpr xs) environment
                    oldenv <- liftState get -- TODO clean this up
                    liftState $ put finalEnvironment
                    retval <- evalStatement (StutterSexpr expression)
                    liftState $ put oldenv
                    return retval
                ("...":_) -> liftExcept $ throwError "incorrect vararg syntax"
                [] -> liftExcept $ throwError "no more arguments to apply"
                [variable] -> do
                    let finalEnvironment = addToEnvironment variable (head xs) environment
                    oldenv <- liftState get -- TODO clean this up
                    liftState $ put finalEnvironment
                    retval <- evalStatement (StutterSexpr expression)
                    liftState $ put oldenv
                    return retval
                (variable:_) -> return $ StutterFunction (tail lambdaArgs, expression, addToEnvironment variable (head xs) environment)
            _ -> liftExcept $ throwError "s-expr should start with function"
evalStatement (StutterSymbol symbol) = lookupEnvironment symbol
evalStatement a = return a

lookupEnvironment :: Symbol -> TransformerStack Expr
lookupEnvironment symbol = do
    environment <- liftState get
    let result = lookupEnvironment' symbol environment
    case result of
        (Just x) -> evalStatement x
        Nothing  -> liftExcept $ throwError "undefined variable"

lookupEnvironment' :: Symbol -> Environment -> Maybe Expr
lookupEnvironment' symbol (Environment variableMap Nothing) = Map.lookup symbol variableMap
lookupEnvironment' symbol (Environment variableMap (Just parentEnv)) = Map.lookup symbol variableMap <|> lookupEnvironment' symbol parentEnv
