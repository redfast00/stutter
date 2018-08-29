module Types (Expr(..), ErrorMessage, createEnvironment, defineVariable, emptyEnvironment, addToEnvironment, lookupEnvironment, pushEnvironment, popEnvironment,  Symbol, TransformerStack, TransformerStackResult, liftExcept, liftIO, liftReader, liftState, runTransformerStack, Environment, EnvStack) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State (get, put, modify, StateT, runStateT)
import           Control.Applicative ((<|>))
import qualified Data.HashMap.Strict  as Map

import           MBot


type Symbol = String
type Environment = Map.HashMap Symbol Expr
type EnvStack = [Environment]

-- TODO use stack of environments instead of maybe Environment

emptyEnvironment = Map.empty

createEnvironment :: [(Symbol, Expr)] -> Environment
createEnvironment x = Map.fromList x

pushEnvironment :: Environment -> TransformerStack ()
pushEnvironment env = liftState $ modify $ \x -> env:x

popEnvironment :: TransformerStack Environment
popEnvironment = do
    a <- liftState get
    return $ head a

addToEnvironment :: Symbol -> Expr -> TransformerStack ()
addToEnvironment symbol expr = do
    env <- liftState get
    case env of
        [] -> liftExcept $ throwError "no env??"
        (e:rest) -> do
            let new = defineVariable symbol expr e
            liftState $ put (new:rest)
    return ()

defineVariable :: Symbol -> Expr -> Environment -> Environment
defineVariable = Map.insert

lookupEnvironment :: Symbol -> TransformerStack Expr
lookupEnvironment symbol = do
    env <- liftState get
    let result = foldl1 (<|>) (fmap (Map.lookup symbol) env)
    case result of
        Nothing -> liftExcept $ throwError "undefined variable"
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
    show (StutterFunction _) = "\\function" -- TODO improve this

type ErrorMessage = String
-- Monad transformer stack
type TransformerStack a = ReaderT Device (StateT EnvStack (ExceptT ErrorMessage IO)) a

type TransformerStackResult a = Either ErrorMessage (a, EnvStack)

runTransformerStack :: Device -> EnvStack -> TransformerStack a -> IO (TransformerStackResult a)
runTransformerStack device environment action = runExceptT $ runStateT (runReaderT action device) environment

-- Future proof when adding more monad transformers
liftReader :: ReaderT Device (StateT EnvStack (ExceptT ErrorMessage IO)) a -> TransformerStack a
liftReader = id

liftState :: StateT EnvStack (ExceptT ErrorMessage IO) a -> TransformerStack a
liftState = lift

liftExcept :: ExceptT ErrorMessage IO a -> TransformerStack a
liftExcept = lift . lift

-- liftIO :: IO a -> TransformerStack a
-- liftIO = lift . lift . lift
