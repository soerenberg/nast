module Text.Nast.Expr
 ( Expr (..)
 , annotate
 ) where


data Expr a = NumLiteral Integer (Maybe Integer) (Maybe Integer)
            | Annotate a (Expr a) deriving (Eq, Show)


annotate :: (Expr a) -> [a] -> (Expr a)
annotate = foldr Annotate
