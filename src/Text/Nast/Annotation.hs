module Text.Nast.Annotation
  ( Annotation (..)
  ) where

data Annotation = LineBased String
                | Bracketed String
                | Newline deriving (Eq, Show)
