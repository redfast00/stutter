module Types (Builtin, Expr(..), ErrorMessage, emptyExpr, throwStutterError, deepAddToEnvironment,  tryStack, createEnvironment, defineVariable, emptyEnvironment, addToEnvironment, lookupEnvironment, pushEnvironment, popEnvironment,  Symbol, TransformerStack, TransformerStackResult, liftExcept, liftIO, liftState, runTransformerStack, Environment, EnvStack) where

import           Control.Applicative  ((<|>))
import           Control.Monad.Except
import           Control.Monad.State  (StateT, get, modify, put, runStateT)
import qualified Data.HashMap.Strict  as Map


type Symbol = String
type Environment = Map.HashMap Symbol Expr
type EnvStack = [Environment]
type Builtin = [Expr] -> TransformerStack Expr


emptyEnvironment :: Environment
emptyEnvironment = Map.empty

createEnvironment :: [(Symbol, Expr)] -> Environment
createEnvironment = Map.fromList

pushEnvironment :: Environment -> TransformerStack ()
pushEnvironment env = liftState $ modify $ \x -> env:x

popEnvironment :: TransformerStack Environment
popEnvironment = do
    a <- liftState get
    liftState $ modify tail
    return $ head a

addToEnvironment :: Symbol -> Expr -> TransformerStack ()
addToEnvironment symbol expr = do
    env <- liftState get
    case env of
        [] -> throwStutterError "No environment present. We messed up, please open an issue"
        (e:rest) -> do
            let new = defineVariable symbol expr e
            liftState $ put (new:rest)
    return ()

deepAddToEnvironment :: Symbol -> Expr -> TransformerStack ()
deepAddToEnvironment symbol expr = do
    env <- liftState get
    case env of
        [] -> liftExcept $ throwError "no env??"
        _ -> liftState $ put $ fmap (defineVariable symbol expr) env
    return ()

defineVariable :: Symbol -> Expr -> Environment -> Environment
defineVariable = Map.insert

lookupEnvironment :: Symbol -> TransformerStack Expr
lookupEnvironment symbol = do
    env <- liftState get
    let result = foldl1 (<|>) (fmap (Map.lookup symbol) env)
    case result of
        Nothing -> throwStutterError $ "undefined variable: " ++ show symbol
        (Just x) -> return x

data Expr = StutterSexpr [Expr]  |
            StutterFexpr [Expr]  |
            StutterNumber Double |
            StutterSymbol Symbol |
            StutterString String |
            StutterBuiltin ([Expr] -> TransformerStack Expr) |
            StutterFunction ([Symbol], [Expr], Environment)

instance Show Expr where
    show (StutterSexpr xs)   = "(" ++ unwords (fmap show xs) ++ ")"
    show (StutterFexpr xs)   = "[" ++ unwords (fmap show xs) ++ "]"
    show (StutterNumber x)   = show x
    show (StutterSymbol x)   = "S=" ++ x
    show (StutterString x)   = show x
    show (StutterBuiltin _)  = "\\builtin"
    show (StutterFunction (symbols, _, _)) = "\\function " ++ show symbols -- TODO improve this

emptyExpr :: Expr
emptyExpr = StutterSexpr []

type ErrorMessage = String
-- Monad transformer stack
type TransformerStack a = (StateT EnvStack (ExceptT ErrorMessage IO)) a

type TransformerStackResult a = Either ErrorMessage (a, EnvStack)

runTransformerStack :: EnvStack -> TransformerStack a -> IO (TransformerStackResult a)
runTransformerStack environment action = runExceptT $ runStateT action environment

-- Futureproof when adding more monad transformers

liftState :: StateT EnvStack (ExceptT ErrorMessage IO) a -> TransformerStack a
liftState = id

liftExcept :: ExceptT ErrorMessage IO a -> TransformerStack a
liftExcept = lift

tryStack :: TransformerStack a -> a -> TransformerStack a
tryStack action defaultReturn = do
    env <- liftState get
    result <- liftIO $ runTransformerStack env action
    case result of
        Left err -> do
            liftIO $ putStrLn err
            return defaultReturn
        Right (r, stack) -> do
            liftState $ put stack
            return r

throwStutterError :: ErrorMessage -> TransformerStack a
throwStutterError message = liftExcept $ throwError message

-- liftIO :: IO a -> TransformerStack a
-- liftIO = lift . lift . lift
