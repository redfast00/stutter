module Builtins (defaultEnvironmentStack) where

import           Types

import           Control.Monad.Except
import           Control.Monad.State

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
lengthCheck exprs expected = if expected ==  length exprs
    then return ()
    else liftExcept $ throwError $ "Incorrect amount of arguments, expected " ++ show expected

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
    return $ StutterFunction (unpackedArgs, function, emptyEnvironment)

defBuiltin :: [Expr]-> TransformerStack Expr
defBuiltin exprs = case exprs of
    (varlist:values@(_:_)) -> do
        vars <- checkFexpr varlist
        lengthCheck values (length vars)
        unpackedVars <- mapM checkSymbol vars
        mapM_ (uncurry addToEnvironment) (zip unpackedVars values)
        return $ StutterSexpr []
    _ -> liftExcept $ throwError "Need at least two arguments"

showBuiltin :: [Expr] -> TransformerStack Expr
showBuiltin exprs = do
    lengthCheck exprs 1
    liftIO $ print (head exprs)
    return $ StutterSexpr []

builtins = [
    ("lifeTheUniverse", StutterNumber 42),
    ("+", StutterBuiltin addBuiltin),
    ("-", StutterBuiltin subBuiltin),
    ("*", StutterBuiltin mulBuiltin),
    ("/", StutterBuiltin divBuiltin),
    ("\\", StutterBuiltin lambdaBuiltin),
    ("def", StutterBuiltin defBuiltin),
    ("show", StutterBuiltin showBuiltin)
    ]
defaultEnvironmentStack = [createEnvironment builtins]
