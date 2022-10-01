module Text.Nast.Parsers.Comments
  ( comment
  ) where

import Text.Nast.Comment (Comment (..))
import Text.Nast.Parsers.Shared (eol)

import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , (<?>)
  , anyChar
  , manyTill
  , noneOf
  , string
  , try)


comment :: Parser Comment
comment =   try lineBased
        <|> try bracketed
        <?> "comment"

lineBased :: Parser Comment
lineBased = do _ <- string "//"
               c <- manyTill (noneOf "\n") eol
               return $ LineBased c

bracketed :: Parser Comment
bracketed = do _ <- string "/*"
               c <- manyTill anyChar (try $ string "*/")
               return $ Bracketed c
