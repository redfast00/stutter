module StutterEvaluator (evalStatement) where

import           Types

evalStatement :: Expr -> TransformerStack Expr
evalStatement x@(StutterSexpr _) = do
    liftIO $ putStrLn "test"
    return $ StutterString "executed sexpr"
evalStatement a = return a
