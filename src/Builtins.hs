module Builtins (defaultEnvironmentStack) where

import           Types

import           Data.Char (chr, ord)

checkNumber :: Expr -> TransformerStack Double
checkNumber (StutterNumber a) = return a
checkNumber q                 = throwStutterError $ "not a number: " ++ show q

checkFexpr :: Expr -> TransformerStack [Expr]
checkFexpr (StutterFexpr a) = return a
checkFexpr q                = throwStutterError $ "not a fexpr: " ++ show q

checkSymbol :: Expr -> TransformerStack Symbol
checkSymbol (StutterSymbol a) = return a
checkSymbol q                 = throwStutterError $ "not a symbol: " ++ show q

lengthCheck :: [Expr] -> Int -> TransformerStack ()
lengthCheck exprs expected = if expected ==  length exprs
    then return ()
    else throwStutterError $ "Incorrect amount of arguments, expected " ++ show expected

binOp :: [Expr] -> TransformerStack (Double, Double)
binOp exprs = do
    lengthCheck exprs 2
    a <- checkNumber (head exprs)
    b <- checkNumber (head (tail exprs))
    return (a, b)

addBuiltin :: Builtin
addBuiltin exprs = do
    (a, b) <- binOp exprs
    return $ StutterNumber (a + b)

subBuiltin :: Builtin
subBuiltin exprs = do
    (a, b) <- binOp exprs
    return $ StutterNumber (a - b)

mulBuiltin :: Builtin
mulBuiltin exprs = do
    (a, b) <- binOp exprs
    return $ StutterNumber (a * b)

divBuiltin :: Builtin
divBuiltin exprs = do
    (a, b) <- binOp exprs
    case b of
        0 -> throwStutterError "Can't divide by zero"
        _ -> return $ StutterNumber (a / b)
        
modBuiltin :: Builtin
modBuiltin exprs = do
    (a, b) <- binOp exprs
    case b of
        0 -> throwStutterError "Can't divide by zero"
        _ -> return $ StutterNumber $ fromIntegral $ mod (round a) $ round b

lambdaBuiltin :: Builtin
lambdaBuiltin exprs = do
    lengthCheck exprs 2
    args <- checkFexpr (head exprs)
    function <- checkFexpr (head (tail exprs))
    unpackedArgs <- mapM checkSymbol args
    return $ StutterFunction (unpackedArgs, function, emptyEnvironment)

defBuiltin :: Builtin
defBuiltin exprs = case exprs of
    (varlist:values@(_:_)) -> do
        vars <- checkFexpr varlist
        lengthCheck values (length vars)
        unpackedVars <- mapM checkSymbol vars
        mapM_ (uncurry addToEnvironment) (zip unpackedVars values)
        return $ StutterSexpr []
    _ -> throwStutterError "def: need at least two arguments"

deepDefBuiltin :: Builtin
deepDefBuiltin exprs = case exprs of
    (varlist:values@(_:_)) -> do
        vars <- checkFexpr varlist
        lengthCheck values (length vars)
        unpackedVars <- mapM checkSymbol vars
        mapM_ (uncurry deepAddToEnvironment) (zip unpackedVars values)
        return $ StutterSexpr []
    _ -> throwStutterError "deepdef: need at least two arguments"

showBuiltin :: Builtin
showBuiltin exprs = do
    lengthCheck exprs 1
    liftIO $ print (head exprs)
    return $ StutterSexpr []

fexprOp :: [Expr] -> TransformerStack [Expr]
fexprOp exprs = do
    lengthCheck exprs 1
    checkFexpr (head exprs)

evalBuiltin :: Builtin
evalBuiltin exprs = StutterSexpr <$> fexprOp exprs


headBuiltin :: Builtin
headBuiltin exprs = do
    lst <- fexprOp exprs
    case lst of
        []    -> throwStutterError "Cannot get head on empty fexpr"
        (x:_) -> return x

tailBuiltin :: Builtin
tailBuiltin exprs = StutterFexpr . tail <$> fexprOp exprs

initBuiltin :: Builtin
initBuiltin exprs = StutterFexpr . init <$> fexprOp exprs

lastBuiltin :: Builtin
lastBuiltin exprs = do
    lst <- fexprOp exprs
    case lst of
        [] -> throwStutterError "Cannot get last on empty fexpr"
        _  -> return $ last lst

firstBuiltin :: Builtin
firstBuiltin exprs = do
    lst <- fexprOp exprs
    case lst of
        []    -> throwStutterError "Cannot get first on empty fexpr"
        (x:_) -> return $ StutterFexpr [x]

ifBuiltin :: Builtin
ifBuiltin [StutterNumber s, iffalse@(StutterFexpr _), iftrue@(StutterFexpr _)] = case s of
    0 -> evalBuiltin [iffalse]
    _ -> evalBuiltin [iftrue]
ifBuiltin args = throwStutterError $ "if needs three arguments: number, fexpr, fexpr. Got: " ++ show args

orderingToNumber :: Ordering -> Double
orderingToNumber LT = -1
orderingToNumber EQ = 0
orderingToNumber GT = 1

-- TODO: compare other types in prelude
compareBuiltin :: Builtin
compareBuiltin [StutterNumber a, StutterNumber b] =
    return $ StutterNumber $ orderingToNumber (compare a b)
compareBuiltin [StutterString a, StutterString b] =
    return $ StutterNumber $ orderingToNumber (compare a b)
compareBuiltin _ = throwStutterError "Can only compare numbers and strings"

emptyBuiltin :: Builtin
emptyBuiltin [StutterFexpr []] = return $ StutterNumber 1
emptyBuiltin _                 = return $ StutterNumber 0

prependBuiltin :: Builtin
prependBuiltin [a, StutterFexpr l] = return $ StutterFexpr $ a:l
prependBuiltin _ = throwStutterError "prepend: takes a f-expr and an expression"

readBuiltin :: Builtin
readBuiltin [StutterNumber 0] = do
    result <- liftIO getLine
    return $ StutterString result
readBuiltin _ = throwStutterError "read: not supported"

fexpr2strBuiltin :: Builtin
fexpr2strBuiltin [StutterFexpr args] = do
        result <- mapM char args
        return $ StutterString result
        where char (StutterNumber a)
                | a == fromInteger (round a) && a >= 0 = return $ chr (round a :: Int)
                | otherwise = throwStutterError "Cannot convert non-natural number to char"
              char _ = throwStutterError "fexpr2str: needs a number"
fexpr2strBuiltin _ = throwStutterError "fexpr2str: takes a fexpr"

str2fexprBuiltin :: Builtin
str2fexprBuiltin [StutterString characters] = return $ StutterFexpr $ map (StutterNumber . fromIntegral . ord) characters
str2fexprBuiltin _ = throwStutterError "str2fexpr: takes a string"

tostring :: Builtin
tostring [variable] = return $ StutterString $ show variable
tostring _ = throwStutterError "tostring: takes one argument"

defaultEnvironmentStack :: EnvStack
defaultEnvironmentStack =
    [createEnvironment builtins]
    where builtins = [
                        ("lifeTheUniverse", StutterNumber 42),
                        ("iadd", StutterBuiltin addBuiltin),
                        ("isub", StutterBuiltin subBuiltin),
                        ("imul", StutterBuiltin mulBuiltin),
                        ("idiv", StutterBuiltin divBuiltin),
                        ("imod", StutterBuiltin modBuiltin),
                        ("\\", StutterBuiltin lambdaBuiltin),
                        ("def", StutterBuiltin defBuiltin),
                        ("show", StutterBuiltin showBuiltin),
                        ("eval", StutterBuiltin evalBuiltin),
                        ("head", StutterBuiltin headBuiltin),
                        ("tail", StutterBuiltin tailBuiltin),
                        ("init", StutterBuiltin initBuiltin),
                        ("last", StutterBuiltin lastBuiltin),
                        ("if", StutterBuiltin ifBuiltin),
                        ("cmp", StutterBuiltin compareBuiltin),
                        ("empty", StutterBuiltin emptyBuiltin),
                        (":", StutterBuiltin prependBuiltin),
                        ("first", StutterBuiltin firstBuiltin),
                        ("deepdef", StutterBuiltin deepDefBuiltin),
                        ("read", StutterBuiltin readBuiltin),
                        ("fexpr-str", StutterBuiltin fexpr2strBuiltin),
                        ("str-fexpr", StutterBuiltin str2fexprBuiltin),
                        ("tostring", StutterBuiltin tostring)
                     ]
