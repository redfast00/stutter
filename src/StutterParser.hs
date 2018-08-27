module StutterParser (exprParse) where

import           Control.Applicative (many, optional, some, (<|>))
import           Data.Char
import           Data.Maybe

import           Parser
import           Types

-- TODO: comments
exprParse :: Parser Expr
exprParse = useFirstParser [numberParser, sexprParse, fexprParse, stringParse, symbolParse]

numberParser :: Parser Expr
numberParser = do
    f <- numbers
    s <- commaPart <|> return [] -- TODO: use optional and allow negative numbers
    return $ StutterNumber (read (f ++ s) :: Double)

numbers :: Parser String
numbers = some (sat isDigit)

commaPart :: Parser String
commaPart = do
    f <- character '.'
    s <- numbers
    return $ f:s

separatedExprParse :: Parser [Expr]
separatedExprParse = do
    space
    expressions <- many (do
        f <- exprParse
        separation
        return f
                        )
    last_expression <- optional exprParse
    space
    return $ expressions ++ maybeToList last_expression

sexprParse :: Parser Expr
sexprParse = do
    character '('
    expressions <- separatedExprParse
    character ')'
    return $ StutterSexpr expressions

fexprParse :: Parser Expr
fexprParse = do
    character '['
    expressions <- separatedExprParse
    character ']'
    return $ StutterFexpr expressions

stringParse :: Parser Expr
stringParse = do
    character '"'
    content <- many stringCharacterParse -- TODO: \n
    character '"'
    return $ StutterString content

stringCharacterParse :: Parser Char
stringCharacterParse = sat (`notElem` "\\\"") <|> (character '\\' >> sat (`elem` "\\\""))

symbolParse :: Parser Expr
symbolParse = do
    name <- some (sat (\x -> isLetter x || (x `elem` "_+-/*\\&|<=>"))) -- TODO maybe allow even more characters
    return $ StutterSymbol name
