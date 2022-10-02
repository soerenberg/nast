module Text.Nast.Parsers.Literals
  ( literal
  , numLiteral
  , signedInt
  , stringLiteral
  ) where

import Text.Nast.Expr (Expr (..))
import Text.Nast.Annotation (Annotation (..))

import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , char
  , digit
  , many
  , many1
  , noneOf
  , optionMaybe)


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
