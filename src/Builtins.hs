module Builtins where

import           Types

import           Control.Monad.Except

checkNumber :: Expr -> TransformerStack Expr
checkNumber x@(StutterNumber _) = return x
checkNumber _ = liftExcept $ throwError "test"

plusBuiltin :: [Expr] -> TransformerStack Expr
-- Check that all numbers, length 2 and then add them
plusBuiltin exprs = do
    checkNumber (head exprs)
    checkNumber (head (tail exprs))
