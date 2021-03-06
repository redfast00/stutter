{-# LANGUAGE LambdaCase #-}

module Parser (Parser, character, sat, skipMany, useFirstParser, parse, parseStatement) where

import           Control.Applicative
import           Control.Monad
-- import           Data.Char

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

parseStatement :: String -> Parser a -> Either String a
parseStatement a parser = case parse parser a of
    [(x,"")] -> Right x
    []       -> Left "no parse possible"
    _        -> Left "parsing failed"

apply :: Parser a -> String -> [(a,String)]
apply = parse

sat  :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

character :: Char -> Parser Char
character c = sat (==c)

item :: Parser Char
item  = Parser (\case
                    ""     -> []
                    (c:cs) -> [(c,cs)])

useFirstParser :: [Parser a] -> Parser a
useFirstParser = foldl1 (<|>)

skipMany :: Parser a -> Parser ()
skipMany p = do
    _ <- many p
    return ()

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a, cs') <- parse p cs])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance MonadPlus Parser where
    mzero = Parser (const [])
    m1 `mplus` m2 = Parser (\s -> apply m1 s ++ apply m2 s)

instance Alternative Parser where
    -- When there are multiple parsing options, select the first one
    p1 <|> p2 = Parser $ \s -> case apply (mplus p1 p2) s of
                                []    -> []
                                (x:_) -> [x]
    empty = mzero
