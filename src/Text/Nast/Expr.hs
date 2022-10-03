module Text.Nast.Expr
 ( Expr (..)
 ) where


data Expr a = NumLiteral            -- ^ Numeric literal
                String              -- ^ Digits before comma
                (Maybe String)      -- ^ Decimal places after comma
                (Maybe String)      -- ^ Exponent
            | StringLiteral String  -- ^ String literal
            | Parens                -- ^ Parentheses expression
                [a]                 -- ^ Annotations after opening `(`
                (Expr a)            -- ^ Expression to be parenthesized
            | Annotate              -- ^ Annotation node
                (Expr a)            -- ^ Expression to be annotated
                a                   -- ^ Annotation
            deriving (Eq, Show)
