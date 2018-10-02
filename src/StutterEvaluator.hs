module StutterEvaluator (evalStatement) where

import           Types

import           Control.Monad.Except

evalStatement :: Expr -> TransformerStack Expr
evalStatement s@(StutterSexpr []) = return s
evalStatement (StutterSexpr [x]) = evalStatement x
evalStatement (StutterSexpr (x:arguments)) = do
        function <- evalStatement x
        case function of
            (StutterBuiltin builtin) -> do
                evaluatedArgs <- mapM evalStatement arguments
                result <- builtin evaluatedArgs
                evalStatement result
            (StutterFunction (lambdaArgs, expression, environment)) -> case lambdaArgs of
                ["...", collector] -> do
                    xxs <- mapM evalStatement arguments
                    let finalEnvironment = defineVariable collector (StutterFexpr xxs) environment
                    evalFunction finalEnvironment expression
                ("...":_) -> throwStutterError "incorrect vararg syntax"
                [] -> throwStutterError "no more arguments to apply"
                [variable] -> do
                    value <- evalStatement $ head arguments
                    let finalEnvironment = defineVariable variable value environment
                    retval <- evalFunction finalEnvironment expression
                    evalStatement $ StutterSexpr $ retval : tail arguments
                (variable:_) -> do
                    value <- evalStatement $ head arguments
                    let newenv = defineVariable variable value environment
                    evalStatement $ StutterSexpr $ StutterFunction (tail lambdaArgs, expression, newenv) : tail arguments
            _ -> throwStutterError "s-expr should start with function"
evalStatement (StutterSymbol symbol) = lookupEnvironment symbol
evalStatement a = return a

evalFunction :: Environment -> [Expr] -> TransformerStack Expr
evalFunction finalEnvironment expression = do
    pushEnvironment finalEnvironment
    retval <- evalStatement (StutterSexpr expression)
    _ <- popEnvironment
    return retval
