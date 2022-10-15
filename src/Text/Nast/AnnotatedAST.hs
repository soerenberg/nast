module Text.Nast.AnnotatedAST
 ( Expr (..)
 , Stmt (..)
 , CodeAnnotation (..)
 , Annotations
 ) where


-- | Expression AST nodes with code annotations
data Expr = Conditional       -- ^ Ternary @?:@ conditional
            Expr              -- ^ Left-hand side (before @?@)
            Annotations       -- ^ succeeding @?@ symbol
            Expr              -- ^ Mid expression (after @?@, before @:@)
            Annotations       -- ^ succeeding @:@ symbol
            Expr              -- ^ Right-hand side (after @:@)
          | Or                -- ^ Logical "or" disjunction (@!!@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @||@ token
            Expr              -- ^ Right-hand side
          | And               -- ^ Logical "and" conjunction (@&&@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @&&@ token
            Expr              -- ^ Right-hand side
          | Equal             -- ^ Equal comparison (@==@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @==@ token
            Expr              -- ^ Right-hand side
          | NotEqual          -- ^ Not-equal comparison (@!=@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @!=@ token
            Expr              -- ^ Right-hand side
          | Gt                -- ^ Greater than (@>@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @>@ token
            Expr              -- ^ Right-hand side
          | Geq               -- ^ Greater or equal than (@>=@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @>=@ token
            Expr              -- ^ Right-hand side
          | Lt                -- ^ Less than (@<@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @<@ token
            Expr              -- ^ Right-hand side
          | Leq               -- ^ Less or equal than (@<=@)
            Expr              -- ^ Left-hand side
            Annotations       -- ^ succeeding @<=@ token
            Expr              -- ^ Right-hand side
          | Add               -- ^ Addition (@+@)
            Expr              -- ^ left summand
            Annotations       -- ^ succeeding @+@ token
            Expr              -- ^ right summand
          | Sub               -- ^ Subtraction (@-@)
            Expr              -- ^ left summand
            Annotations       -- ^ succeeding @-@ token
            Expr              -- ^ right summand
          | Mul               -- ^ Multiplication (@*@)
            Expr              -- ^ left factor
            Annotations       -- ^ succeeding @*@ token
            Expr              -- ^ right factor
          | Div               -- ^ Division (@/@)
            Expr              -- ^ left factor
            Annotations       -- ^ succeeding @/@ token
            Expr              -- ^ right factor
          | EltMul            -- ^ Element-wise multiplication (@.*@)
            Expr              -- ^ left factor
            Annotations       -- ^ succeeding @.*@ token
            Expr              -- ^ right factor
          | EltDiv            -- ^ Element-wise division (@./@)
            Expr              -- ^ left factor
            Annotations       -- ^ succeeding @./@ token
            Expr              -- ^ right factor
          | LDiv              -- ^ Left-division (@\\@)
            Expr              -- ^ left factor
            Annotations       -- ^ succeeding @\\@ token
            Expr              -- ^ right factor
          | IntDiv            -- ^ Integer division (@%\\%@)
            Expr              -- ^ left factor
            Annotations       -- ^ succeeding @%\\%@ token
            Expr              -- ^ right factor
          | LogicalNeg        -- ^ Logical negation (@!@)
            Annotations       -- ^ succeeding @!@ token
            Expr              -- ^ Right-hand side
          | UnaryPlus         -- ^ Promotion (prefix @+@), no-op
            Annotations       -- ^ succeeding @+@ token
            Expr              -- ^ Right-hand side
          | UnaryMinus        -- ^ Arithmetic negation (prefix @-@)
            Annotations       -- ^ succeeding @-@ token
            Expr              -- ^ Right-hand side
          | Pow               -- ^ Exponentiation (@^@)
            Expr              -- ^ base
            Annotations       -- ^ succeeding @^@ token
            Expr              -- ^ exponent
          | EltPow            -- ^ Element-wise exponentiation (@.^@)
            Expr              -- ^ base
            Annotations       -- ^ succeeding @.^@ token
            Expr              -- ^ exponent
          | Transpose         -- ^ Matrix transposition
            Expr              -- ^ expression to be transposed
            Annotations       -- ^ succeeding @'@ token
          | Call              -- ^ Function application
            Expr              -- ^ callee
            [Expr]            -- ^ arguments
            [Annotations]     -- ^ annotations preceding each argement
            Annotations       -- ^ succeeding closing @)@ token
          | Index             -- ^ Array indexing
            Expr              -- ^ expression to index
            [Expr]            -- ^ indices
            [Annotations]     -- ^ annotations preceding each index
            Annotations       -- ^ succeeding closing @]@ token
          | Range             -- ^ Range, e.g. @1:9@, or @:3@
            (Maybe Expr)      -- ^ lower limit
            Annotations       -- ^ succeeding closing @:@ token
            (Maybe Expr)      -- ^ upper limit
          | NumLiteral        -- ^ Numeric literal
            String            -- ^ Digits before comma
            (Maybe String)    -- ^ Decimal places after comma
            (Maybe String)    -- ^ Exponent
            Annotations       -- ^ succeeding the literal
          | StringLiteral     -- ^ String literal
            String            -- ^ literal
            Annotations       -- ^ succeeding the literal
          | Printables        -- ^ printables
            [Expr]            -- ^ one or more expressions
            [Annotations]     -- ^ annotations preceding each printable
          | Parens            -- ^ Parentheses expression
            Annotations       -- ^ succeeding the opening @(@
            Expr              -- ^ Expression to be parenthesized
            Annotations       -- ^ succeeding the closing @)@
          | Identifier        -- ^ Identifier
            String            -- ^ name
            Annotations       -- ^ succeeding the identifier
          deriving (Eq, Show)


-- | Statement AST nodes with code annotations
data Stmt = Break             -- ^ @break@ statement
            Annotations       -- ^ succeeding the @break@ keyword
            Annotations       -- ^ succeeding the @;@ symbol
          | Continue          -- ^ @continue@ statement
            Annotations       -- ^ succeeding the @continue@ keyword
            Annotations       -- ^ succeeding the @;@ symbol
          | Return            -- ^ @return@ statement
            Annotations       -- ^ succeeding the @return@ keyword
            Annotations       -- ^ succeeding the @;@ symbol
          | Block             -- ^ Block statement (@{..}@)
            Annotations       -- ^ succeeding the opening @{@
            [Stmt]            -- ^ statements inside curly braces
            Annotations       -- ^ succeeding the closing @}@
          | If                -- ^ If statement (without else clause)
            Annotations       -- ^ succeeding the @if@ keyword
            Expr              -- ^ conditional expression
            Stmt              -- ^ "then" statement
          | IfElse            -- ^ If / else statement
            Annotations       -- ^ succeeding the @if@ keyword
            Expr              -- ^ conditional expression
            Stmt              -- ^ "then" statement
            Annotations       -- ^ succeeding the @else@ keyword
            Stmt              -- ^ "else" statement
          | Assign            -- ^ assignment (@=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | ArrowAssign       -- ^ assignment (@<-@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @<-@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | PlusAssign        -- ^ plus-assignment (@+=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @+=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | MinusAssign       -- ^ minus-assignment (@-=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @-=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | TimesAssign       -- ^ times-assignment (@*=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @*=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | DivideAssign      -- ^ divide-assignment (@/=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @/=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | EltTimesAssign    -- ^ element-wise times-assignment (@.*=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @.*=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | EltDivideAssign   -- ^ element-wise divide-assignment (@./=@)
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @./=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | TargetPlusAssign  -- ^ increment target (@target += expr;@)
            Annotations       -- ^ succeeding the @target@ keyword
            Annotations       -- ^ succeeding the @+=@ symbol
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@ symbol
          | For               -- ^ for loop without range (@for (.. in ..)@)
            Annotations       -- ^ succeeding the @for@ keyword
            Annotations       -- ^ succeeding the opening @(@
            Expr              -- ^ variable, identifier (lhs of @in@)
            Annotations       -- ^ succeeding the @in@ keyword
            Expr              -- ^ expression to iterate over (rhs of @in@)
            Annotations       -- ^ succeeding the closing @)@
            Stmt              -- ^ body
          | ForRange          -- ^ for loop with range (@for (.. in ..:..)@)
            Annotations       -- ^ succeeding the @for@ keyword
            Annotations       -- ^ succeeding the opening @(@
            Expr              -- ^ variable, identifier
            Annotations       -- ^ succeeding the @in@ keyword
            Expr              -- ^ lower bound of range (lhs of @:@)
            Annotations       -- ^ succeeding the @:@
            Expr              -- ^ upper bound of range (rhs of @:@)
            Annotations       -- ^ succeeding the closing @)@
            Stmt              -- ^ body
          | While             -- ^ while loop
            Annotations       -- ^ succeeding the @while@ keyword
            Annotations       -- ^ succeeding the opening @(@
            Expr              -- ^ condition
            Annotations       -- ^ succeeding the closing @)@
            Stmt              -- ^ body
          | Tilde             -- ^ tilde expression
            Expr              -- ^ left-hand side
            Annotations       -- ^ succeeding the @~@
            Expr              -- ^ right-hand side
            Annotations       -- ^ succeeding the @;@
          | Print             -- ^ print statement
            Annotations       -- ^ succeeding the @print@ keyword
            Expr              -- ^ printables
            Annotations       -- ^ succeeding the closing @)@
            Annotations       -- ^ succeeding the @;@
          | Reject            -- ^ print statement
            Annotations       -- ^ succeeding the @reject@ keyword
            Expr              -- ^ printables
            Annotations       -- ^ succeeding the closing @)@
            Annotations       -- ^ succeeding the @;@
          deriving (Eq, Show)


-- | Annotations of source code (comments, linebreaks)
data CodeAnnotation = LineBased String   -- ^ @// ...@ style comment
                    | Bracketed String   -- ^ @//* ... *//@ style comment
                    | Newline            -- ^ Line break
                    deriving (Eq, Show)

-- | Type synonym, zero or more code annotations
type Annotations = [CodeAnnotation]
