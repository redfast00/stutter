module Builtins (defaultEnvironment) where

import           Types

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict  as Map

checkNumber :: Expr -> TransformerStack Double
checkNumber (StutterNumber a) = return a
checkNumber q                 = liftExcept $ throwError $ "not a number" ++ show q

checkFexpr :: Expr -> TransformerStack [Expr]
checkFexpr (StutterFexpr a) = return a
checkFexpr q                = liftExcept $ throwError $ "not a fexpr" ++ show q

checkSymbol :: Expr -> TransformerStack Symbol
checkSymbol (StutterSymbol a) = return a
checkSymbol q                 = liftExcept $ throwError $ "not a symbol" ++ show q

lengthCheck :: [Expr] -> Int -> TransformerStack ()
lengthCheck exprs expected = case length exprs of
    expected -> return ()
    _ -> liftExcept $ throwError $ "Incorrect amount of arguments, expected " ++ show expected

binOp :: [Expr] -> TransformerStack (Double, Double)
binOp exprs = do
    lengthCheck exprs 2
    a <- checkNumber (head exprs)
    b <- checkNumber (head (tail exprs))
    return (a, b)

addBuiltin exprs = do
    (a, b) <- binOp exprs
    return $ StutterNumber (a + b)

subBuiltin exprs = do
    (a, b) <- binOp exprs
    return $ StutterNumber (a - b)

mulBuiltin exprs = do
    (a, b) <- binOp exprs
    return $ StutterNumber (a * b)

divBuiltin exprs = do
    (a, b) <- binOp exprs
    case b of
        0 -> liftExcept $ throwError "Can't divide by zero"
        _ -> return $ StutterNumber (a * b)

lambdaBuiltin exprs = do
    lengthCheck exprs 2
    args <- checkFexpr (head exprs)
    function <- checkFexpr (head (tail exprs))
    unpackedArgs <- mapM checkSymbol args
    parentEnv <- liftState get
    return $ StutterFunction (unpackedArgs, function, Environment Map.empty (Just parentEnv))

defBuiltin :: [Expr]-> TransformerStack Expr
defBuiltin exprs = case exprs of
    (varlist:values@(_:_)) -> do
        vars <- checkFexpr varlist
        lengthCheck values (length vars)
        unpackedVars <- mapM checkSymbol vars
        mapM_ defSingle (zip unpackedVars values)
        return $ StutterSexpr []
    _ -> liftExcept $ throwError "Need at least two arguments"

defSingle :: (Symbol, Expr) -> TransformerStack ()
defSingle (symbol, value) = do
    env <- liftState get
    let new = addToEnvironment symbol value env
    liftState $ put new
    return ()

idLambda = StutterFunction (["x"], [StutterSymbol "x"], Environment Map.empty (Just defaultEnvironment))
idVarargs = StutterFunction (["...", "xs"], [StutterSymbol "xs"], Environment Map.empty (Just defaultEnvironment))

builtins = [
    ("id", idLambda),
    ("idv", idVarargs),
    ("lifeTheUniverse", StutterNumber 42),
    ("+", StutterBuiltin addBuiltin),
    ("-", StutterBuiltin subBuiltin),
    ("*", StutterBuiltin mulBuiltin),
    ("/", StutterBuiltin divBuiltin),
    ("\\", StutterBuiltin lambdaBuiltin),
    ("def", StutterBuiltin defBuiltin)
    ]
defaultEnvironment = Environment (Map.fromList builtins) Nothing
