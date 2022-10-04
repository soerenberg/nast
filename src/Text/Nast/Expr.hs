module Text.Nast.Expr
 ( Expr (..)
 ) where


data Expr a = Add                   -- ^ Addition
              (Expr a)              -- ^ left summand
              [a]                   -- ^ Annotations after '+' symbol
              (Expr a)              -- ^ right summand
            | Sub                   -- ^ Subtraction
              (Expr a)              -- ^ left summand
              [a]                   -- ^ Annotations after '-' symbol
              (Expr a)              -- ^ right summand
            | NumLiteral            -- ^ Numeric literal
                String              -- ^ Digits before comma
                (Maybe String)      -- ^ Decimal places after comma
                (Maybe String)      -- ^ Exponent
            | StringLiteral String  -- ^ String literal
            | Parens                -- ^ Parentheses expression
                [a]                 -- ^ Annotations after opening `(`
                (Expr a)            -- ^ Expression to be parenthesized
            | Identifier String     -- ^ Identifier
            | Annotate              -- ^ Annotation node
                (Expr a)            -- ^ Expression to be annotated
                a                   -- ^ Annotation
            deriving (Eq, Show)
