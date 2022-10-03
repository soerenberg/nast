module Text.Nast.Annotation
  ( Annotation (..)
  ) where

-- | Annotation type for AST nodes. Comments or newlines
data Annotation = LineBased String  -- ^ @// ...@ style comment
                | Bracketed String  -- ^ @//* ... *//@ style comment
                | Newline           -- ^ Line break
                deriving (Eq, Show)
