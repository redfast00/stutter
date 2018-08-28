module Types (Expr(..), ErrorMessage, emptyEnvironment, addToEnvironment, Symbol, TransformerStack, liftExcept, liftIO, liftReader, liftState, runTransformerStack, Environment(..)) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict  as Map

import           MBot


type Symbol = String

data Environment = Environment (Map.HashMap Symbol Expr) (Maybe Environment)
    deriving (Show)

-- TODO use stack of environments instead of maybe Environment

emptyEnvironment = Environment Map.empty Nothing

addToEnvironment :: Symbol -> Expr -> Environment -> Environment
addToEnvironment symbol expr (Environment varmap parent) = Environment (Map.insert symbol expr varmap) parent

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
type TransformerStack a = ReaderT Device (StateT Environment (ExceptT ErrorMessage IO)) a

runTransformerStack :: Device -> Environment -> TransformerStack a -> IO (Either ErrorMessage (a, Environment))
runTransformerStack device environment action = runExceptT $ runStateT (runReaderT action device) environment

-- Future proof when adding more monad transformers
liftReader :: ReaderT Device (StateT Environment (ExceptT ErrorMessage IO)) a -> TransformerStack a
liftReader = id

liftState :: StateT Environment (ExceptT ErrorMessage IO) a -> TransformerStack a
liftState = lift

liftExcept :: ExceptT ErrorMessage IO a -> TransformerStack a
liftExcept = lift . lift

-- liftIO :: IO a -> TransformerStack a
-- liftIO = lift . lift . lift
