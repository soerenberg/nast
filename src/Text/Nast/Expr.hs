module Text.Nast.Expr
 ( Expr (..)
 ) where


data Expr a = NumLiteral String (Maybe String) (Maybe String)
            | StringLiteral String
            | Annotate (Expr a) a deriving (Eq, Show)
