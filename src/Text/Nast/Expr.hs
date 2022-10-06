module Text.Nast.Expr
 ( Expr (..)
 ) where


data Expr a = Gt                    -- ^ Greater than (@>@)
              (Expr a)              -- ^ Left-hand side
              [a]                   -- ^ Annotations after @>@ symbol
              (Expr a)              -- ^ Right-hand side
            | Geq                   -- ^ Greater or equal than (@>=@)
              (Expr a)              -- ^ Left-hand side
              [a]                   -- ^ Annotations after @>=@ symbol
              (Expr a)              -- ^ Right-hand side
            | Lt                    -- ^ Less than (@<@)
              (Expr a)              -- ^ Left-hand side
              [a]                   -- ^ Annotations after @<@ symbol
              (Expr a)              -- ^ Right-hand side
            | Leq                   -- ^ Less or equal than (@<=@)
              (Expr a)              -- ^ Left-hand side
              [a]                   -- ^ Annotations after @<=@ symbol
              (Expr a)              -- ^ Right-hand side
            | Add                   -- ^ Addition (@+@)
              (Expr a)              -- ^ left summand
              [a]                   -- ^ Annotations after @+@ symbol
              (Expr a)              -- ^ right summand
            | Sub                   -- ^ Subtraction (@-@)
              (Expr a)              -- ^ left summand
              [a]                   -- ^ Annotations after @-@ symbol
              (Expr a)              -- ^ right summand
            | Mul                   -- ^ Multiplication (@*@)
              (Expr a)              -- ^ left factor
              [a]                   -- ^ Annotations after @*@ symbol
              (Expr a)              -- ^ right factor
            | Div                   -- ^ Division (@/@)
              (Expr a)              -- ^ left factor
              [a]                   -- ^ Annotations after @/@ symbol
              (Expr a)              -- ^ right factor
            | EltMul                -- ^ Element-wise multiplication (@.*@)
              (Expr a)              -- ^ left factor
              [a]                   -- ^ Annotations after @.*@ symbol
              (Expr a)              -- ^ right factor
            | EltDiv                -- ^ Element-wise division (@./@)
              (Expr a)              -- ^ left factor
              [a]                   -- ^ Annotations after @./@ symbol
              (Expr a)              -- ^ right factor
            | LDiv                  -- ^ Left-division (@\\@)
              (Expr a)              -- ^ left factor
              [a]                   -- ^ Annotations after @\\@ symbol
              (Expr a)              -- ^ right factor
            | IntDiv                -- ^ Integer division (@%\\%@)
              (Expr a)              -- ^ left factor
              [a]                   -- ^ Annotations after @%\\%@ symbol
              (Expr a)              -- ^ right factor
            | NumLiteral            -- ^ Numeric literal
                String              -- ^ Digits before comma
                (Maybe String)      -- ^ Decimal places after comma
                (Maybe String)      -- ^ Exponent
            | StringLiteral String  -- ^ String literal
            | Parens                -- ^ Parentheses expression
                [a]                 -- ^ Annotations after opening @(@
                (Expr a)            -- ^ Expression to be parenthesized
            | Identifier String     -- ^ Identifier
            | Annotate              -- ^ Annotation node
                (Expr a)            -- ^ Expression to be annotated
                a                   -- ^ Annotation
            deriving (Eq, Show)
