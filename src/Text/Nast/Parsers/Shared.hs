module Text.Nast.Parsers.Shared
  ( eol
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
eol = (lookAhead eof) <|> (void $ char '\n') <?> "end of line"
