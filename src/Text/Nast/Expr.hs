module Text.Nast.Expr
 ( Expr (..)
 , annotate
 ) where


data Expr a = NumLiteral String (Maybe String) (Maybe String)
            | Annotate a (Expr a) deriving (Eq, Show)


annotate :: (Expr a) -> [a] -> (Expr a)
annotate = foldr Annotate
