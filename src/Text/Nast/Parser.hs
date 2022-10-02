module Text.Nast.Parser (
  -- annotations
    annotation
  , bracketed
  , lineBased
  -- literals
  , literal
  , numLiteral
  , signedInt
  , stringLiteral
  -- shared
  , eol
  , newline
  ) where


import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , (<?>)
  , anyChar
  , char
  , digit
  , eof
  , lookAhead
  , many
  , many1
  , manyTill
  , noneOf
  , optionMaybe
  , string
  , try
  )
import Control.Monad (void)

import Text.Nast.Expr (Expr (..))
import Text.Nast.Annotation (Annotation (..))


annotation :: Parser Annotation
annotation = newline <|> comment

comment :: Parser Annotation
comment =   try lineBased
        <|> try bracketed
        <?> "comment"

bracketed :: Parser Annotation
bracketed = do _ <- string "/*"
               c <- manyTill anyChar (try $ string "*/")
               return $ Bracketed c

lineBased :: Parser Annotation
lineBased = do _ <- string "//"
               c <- manyTill (noneOf "\n") eol
               return $ LineBased c

literal :: Parser (Expr Annotation)
literal = numLiteral <|> stringLiteral

numLiteral :: Parser (Expr Annotation)
numLiteral = do i <- signedInt
                d <- optionMaybe (char '.' >> many1 digit)
                e <- optionMaybe (char 'e' >> signedInt)
                return $ NumLiteral i d e

signedInt :: Parser String
signedInt = do plus <|> minus <|> nosign
    where nosign = many1 digit
          plus = (:) <$> char '+' <*> nosign
          minus = (:) <$> char '-' <*> nosign

stringLiteral :: Parser (Expr Annotation)
stringLiteral = do _ <- char '"'
                   s <- many $ noneOf "\n\""
                   _ <- char '"'
                   return $ StringLiteral s

eol :: Parser ()
eol = (lookAhead eof) <|> (void $ char '\n') <?> "end of line"

newline :: Parser Annotation
newline = char '\n' >> return Newline <?> "newline"
