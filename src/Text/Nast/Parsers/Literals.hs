module Text.Nast.Parsers.Literals
  ( numLiteral
  , signedInt
  ) where

import Text.Nast.Expr (Expr (..))
import Text.Nast.Annotation (Annotation (..))

import Text.ParserCombinators.Parsec
  (Parser
  , (<|>)
  , char
  , digit
  , many1
  , optionMaybe)


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
