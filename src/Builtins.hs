module Builtins (defaultEnvironment) where

import           Types

import           Control.Monad.Except
import qualified Data.HashMap.Strict  as Map

checkNumber :: Expr -> TransformerStack Double
checkNumber x@(StutterNumber a) = return a
checkNumber q                   = liftExcept $ throwError $ "not a number" ++ show q

lenghtCheck :: [Expr] -> Integer -> TransformerStack ()
lenghtCheck exprs expected = case length exprs of
    expected -> return ()
    _ -> liftExcept $ throwError $ "Incorrect amount of arguments, expected " ++ show expected

plusBuiltin :: [Expr] -> TransformerStack Expr
-- Check that all numbers, length 2 and then add them
plusBuiltin exprs = do
    lenghtCheck exprs 2
    a <- checkNumber (head exprs)
    b <- checkNumber (head (tail exprs))
    return $ StutterNumber (a + b)

idLambda = StutterFunction (["x"], [StutterSymbol "x"], Environment Map.empty (Just defaultEnvironment))

builtins = [("id", idLambda), ("lifeTheUniverse", StutterNumber 42), ("+", StutterBuiltin plusBuiltin)]
defaultEnvironment = Environment (Map.fromList builtins) Nothing
