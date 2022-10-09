module Text.Nast.AST
 ( Expr (..)
 , Stmt (..)
 ) where


data Expr a = Conditional           -- ^ Ternary @?:@ conditional
              (Expr a)              -- ^ Left-hand side (before @?@)
              (Expr a)              -- ^ Mid expression (after @?@, before @:@)
              (Expr a)              -- ^ Right-hand side (after @:@)
              a                     -- ^ annotation
            | Or                    -- ^ Logical "or" disjunction (@!!@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | And                   -- ^ Logical "and" conjunction (@&&@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Equal                 -- ^ Equal comparison (@==@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | NotEqual              -- ^ Not-equal comparison (@!=@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Gt                    -- ^ Greater than (@>@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Geq                   -- ^ Greater or equal than (@>=@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Lt                    -- ^ Less than (@<@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Leq                   -- ^ Less or equal than (@<=@)
              (Expr a)              -- ^ Left-hand side
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Add                   -- ^ Addition (@+@)
              (Expr a)              -- ^ left summand
              (Expr a)              -- ^ right summand
              a                     -- ^ annotation
            | Sub                   -- ^ Subtraction (@-@)
              (Expr a)              -- ^ left summand
              (Expr a)              -- ^ right summand
              a                     -- ^ annotation
            | Mul                   -- ^ Multiplication (@*@)
              (Expr a)              -- ^ left factor
              (Expr a)              -- ^ right factor
              a                     -- ^ annotation
            | Div                   -- ^ Division (@/@)
              (Expr a)              -- ^ left factor
              (Expr a)              -- ^ right factor
              a                     -- ^ annotation
            | EltMul                -- ^ Element-wise multiplication (@.*@)
              (Expr a)              -- ^ left factor
              (Expr a)              -- ^ right factor
              a                     -- ^ annotation
            | EltDiv                -- ^ Element-wise division (@./@)
              (Expr a)              -- ^ left factor
              (Expr a)              -- ^ right factor
              a                     -- ^ annotation
            | LDiv                  -- ^ Left-division (@\\@)
              (Expr a)              -- ^ left factor
              (Expr a)              -- ^ right factor
              a                     -- ^ annotation
            | IntDiv                -- ^ Integer division (@%\\%@)
              (Expr a)              -- ^ left factor
              (Expr a)              -- ^ right factor
              a                     -- ^ annotation
            | LogicalNeg            -- ^ Logical negation (@!@)
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | UnaryPlus             -- ^ Promotion (prefix @+@), no-op
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | UnaryMinus            -- ^ Arithmetic negation (prefix @-@)
              (Expr a)              -- ^ Right-hand side
              a                     -- ^ annotation
            | Pow                   -- ^ Exponentiation (@^@)
              (Expr a)              -- ^ base
              (Expr a)              -- ^ exponent
              a                     -- ^ annotation
            | EltPow                -- ^ Element-wise exponentiation (@.^@)
              (Expr a)              -- ^ base
              (Expr a)              -- ^ exponent
              a                     -- ^ annotation
            | Transpose             -- ^ Matrix transposition
              (Expr a)              -- ^ expression to be transposed
              a                     -- ^ annotation
            | Call                  -- ^ Function application
              (Expr a)              -- ^ callee
              [Expr a]              -- ^ arguments
              a                     -- ^ annotation
            | Index                 -- ^ Array indexing
              (Expr a)              -- ^ expression to index
              [Expr a]              -- ^ indices
              a                     -- ^ annotation
            | Range                 -- ^ Range, e.g. @1:9@, or @:3@
              (Maybe (Expr a))      -- ^ lower limit
              (Maybe (Expr a))      -- ^ upper limit
              a                     -- ^ annotation
            | NumLiteral            -- ^ Numeric literal
              String                -- ^ Digits before comma
              (Maybe String)        -- ^ Decimal places after comma
              (Maybe String)        -- ^ Exponent
              a                     -- ^ annotation
            | StringLiteral         -- ^ String literal
              String                -- ^ literal
              a                     -- ^ annotation
            | Parens                -- ^ Parentheses expression
              (Expr a)              -- ^ Expression to be parenthesized
              a                     -- ^ annotation
            | Identifier            -- ^ Identifier
              String                -- ^ name
              a                     -- ^ annotation
            deriving (Eq, Show)


data Stmt a = Break     -- ^ @break@ statement
              a         -- ^ annotation
            | Continue  -- @continue@ statement
              a         -- ^ annotation
            deriving (Eq, Show)