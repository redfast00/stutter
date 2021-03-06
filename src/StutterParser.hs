module StutterParser (fileParse, replLineParse, parseThing) where

import           Control.Applicative (empty, many, optional, some, (<|>))
import           Data.Char
import           Data.Maybe

import           Parser
import           Types

parseThing :: Parser a -> String -> TransformerStack a
parseThing parser input = case parseStatement input parser of
    Left err -> throwStutterError err
    Right value -> return value

replLineParse :: Parser Expr
replLineParse = StutterSexpr <$> separatedExprParse separation

line :: Parser a -> Parser a
line p = do
    x <- p
    _ <- character '\n'
    return x

fileParse :: Parser [Expr]
fileParse = many (do
    _  <- many (comment <|> line empty)
    f <- line replLineParse
    _  <- many (comment <|> line empty)
    return f
                 )

comment :: Parser String
comment = do
    _ <- character '#'
    many $ sat (/= '\n')

whitespace :: Parser Char
whitespace = character ' ' <|> character '\t'

space :: Parser String
space  = many whitespace

separation :: Parser String
separation = some whitespace

newlineSeparation :: Parser String
newlineSeparation = some (whitespace <|> character '\n')

exprParse :: Parser Expr
exprParse = useFirstParser [numberParser, sexprParse, fexprParse, stringParse, symbolParse]

numberParser :: Parser Expr
numberParser = do
    sign <- optional $ character '-'
    f <- numbers
    s <- commaPart <|> return []
    return $ StutterNumber (read (maybeToList sign ++ f ++ s) :: Double)

numbers :: Parser String
numbers = some (sat isDigit)

commaPart :: Parser String
commaPart = do
    f <- character '.'
    s <- numbers
    return $ f:s

separatedBy :: Parser String -> Parser a -> Parser [a]
separatedBy separationParser p = separatedBy1 separationParser p <|> return []

separatedBy1 :: Parser String -> Parser a -> Parser [a]
separatedBy1 separationParser p = do
    f <- p
    rest <- many $ separationParser >> p
    return $ f:rest

separatedExprParse :: Parser String -> Parser [Expr]
separatedExprParse separationParser = do
    _ <- space
    expressions <- many (do
        f <- exprParse
        _ <- separationParser
        return f
                        )
    last_expression <- optional exprParse
    _ <- space
    return $ expressions ++ maybeToList last_expression

sexprParse :: Parser Expr
sexprParse = do
    _ <- character '('
    _ <- optional newlineSeparation
    expressions <- separatedBy newlineSeparation exprParse
    _ <- optional newlineSeparation
    _ <- character ')'
    return $ StutterSexpr expressions

fexprParse :: Parser Expr
fexprParse = do
    _ <- character '['
    _ <- optional newlineSeparation
    expressions <- separatedBy newlineSeparation exprParse
    _ <- optional newlineSeparation
    _ <- character ']'
    return $ StutterFexpr expressions

stringParse :: Parser Expr
stringParse = do
    _ <- character '"'
    content <- many stringCharacterParse -- TODO: \n
    _ <- character '"'
    return $ StutterString content

stringCharacterParse :: Parser Char
stringCharacterParse = sat (`notElem` "\\\"") <|> (character '\\' >> sat (`elem` "\\\""))

symbolParse :: Parser Expr
symbolParse = do
    name <- some (sat (\x -> isLetter x || (x `elem` "_+-/*\\&|<=>.:"))) -- TODO maybe allow even more characters
    return $ StutterSymbol name
