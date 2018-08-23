module Types (Expr(..)) where

import           Data.List (intercalate)

data Expr = StutterSexpr [Expr]  |
            StutterFexpr [Expr]  |
            StutterNumber Double |
            StutterSymbol String |
            StutterString String |
            StutterFunction ([Expr] -> Expr)

instance Show Expr where
    show (StutterSexpr xs)   = "(" ++ unwords (fmap show xs) ++ ")"
    show (StutterFexpr xs)   = "[" ++ unwords (fmap show xs) ++ "]"
    show (StutterNumber x)   = show x
    show (StutterSymbol x)   = "S=" ++ x
    show (StutterString x)   = show x
    show (StutterFunction _) = "\\function"
