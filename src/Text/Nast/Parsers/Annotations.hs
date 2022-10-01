module Text.Nast.Parsers.Annotations
  ( annotation
  , newline
  , comment
  , lineBased
  , bracketed
  ) where

import Text.Nast.Annotation (Annotation (..))
import Text.Nast.Parsers.Shared (eol)

import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , (<?>)
  , anyChar
  , char
  , manyTill
  , noneOf
  , string
  , try)

annotation :: Parser Annotation
annotation = newline <|> comment

newline :: Parser Annotation
newline = char '\n' >> return Newline <?> "newline"

comment :: Parser Annotation
comment =   try lineBased
        <|> try bracketed
        <?> "comment"

lineBased :: Parser Annotation
lineBased = do _ <- string "//"
               c <- manyTill (noneOf "\n") eol
               return $ LineBased c

bracketed :: Parser Annotation
bracketed = do _ <- string "/*"
               c <- manyTill anyChar (try $ string "*/")
               return $ Bracketed c
