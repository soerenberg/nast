module Text.Nast.Parsers.Shared
  ( eol
  , newline
  ) where


import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , (<?>)
  , char
  , eof
  , lookAhead
  )
import Control.Monad (void)

eol :: Parser ()
eol = (lookAhead eof) <|> newline

newline :: Parser ()
newline = (void $ char '\n') <?> "end of line"
