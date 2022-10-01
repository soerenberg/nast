module Text.Nast.Comment
  ( Comment (..)
  ) where

data Comment = LineBased String
             | Bracketed String deriving (Eq, Show)
