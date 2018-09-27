module StutterEvaluator (evalStatement) where

import           Types

import           Control.Monad.Except

evalStatement :: Expr -> TransformerStack Expr
evalStatement s@(StutterSexpr []) = return s
evalStatement (StutterSexpr [x]) = evalStatement x
evalStatement (StutterSexpr (x:xxs)) = do
        xs <- mapM evalStatement xxs
        function <- evalStatement x
        case function of
            (StutterBuiltin builtin) -> do
                evaluatedArgs <- mapM evalStatement xs
                result <- builtin evaluatedArgs
                evalStatement result
            (StutterFunction (lambdaArgs, expression, environment)) -> case lambdaArgs of
                ["...", collector] -> do
                    let finalEnvironment = defineVariable collector (StutterFexpr xs) environment
                    evalFunction finalEnvironment expression
                ("...":_) -> liftExcept $ throwError "incorrect vararg syntax"
                [] -> liftExcept $ throwError "no more arguments to apply"
                [variable] -> do
                    let finalEnvironment = defineVariable variable (head xs) environment
                    evalFunction finalEnvironment expression
                (variable:_) -> do
                    let newenv = defineVariable variable (head xs) environment
                    evalStatement $ StutterSexpr $ StutterFunction (tail lambdaArgs, expression, newenv) : tail xs
            _ -> liftExcept $ throwError "s-expr should start with function"
evalStatement (StutterSymbol symbol) = lookupEnvironment symbol
evalStatement a = return a

evalFunction :: Environment -> [Expr] -> TransformerStack Expr
evalFunction finalEnvironment expression = do
    pushEnvironment finalEnvironment
    retval <- evalStatement (StutterSexpr expression)
    _ <- popEnvironment
    return retval
