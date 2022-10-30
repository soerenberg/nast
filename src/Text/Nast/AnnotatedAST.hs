module Text.Nast.AnnotatedAST
 ( StanProgram (..)
 , ProgramBlock (..)
 , Expr (..)
 , Stmt (..)
 , BinaryOperator (..)
 , CodeAnnotation (..)
 , Annotations
 , VarType (..)
 , ScalarVarType
 , VectorVarType
 , VectorNoConstrVarType
 , MatrixVarType
 , MatrixOptDimsVarType
 , VarConstraints (..)
 , VarConstraint (..)
 , ArrayDims (..)
 ) where


-- | Stan program structure
data StanProgram = StanProgram
  { header                       :: Annotations
  , function_block               :: Maybe ProgramBlock
  , data_block                   :: Maybe ProgramBlock
  , transformed_data_block       :: Maybe ProgramBlock
  , parameters_block             :: Maybe ProgramBlock
  , transformed_parameters_block :: Maybe ProgramBlock
  , model_block                  :: Maybe ProgramBlock
  , generated_quantities_block   :: Maybe ProgramBlock
  } deriving (Eq, Show)


-- | Program block
data ProgramBlock = ProgramBlock
                    Annotations    -- ^ succeeding block name
                    Annotations    -- ^ succeeding @{@
                    [Stmt]         -- ^ statements
                    Annotations    -- ^ succeeding @}@
                  deriving (Eq, Show)


-- | Expression AST nodes with code annotations
data Expr = Conditional       -- ^ Ternary @?:@ conditional
            Expr              -- ^ Left-hand side (before @?@)
            Annotations       -- ^ succeeding @?@ symbol
            Expr              -- ^ Mid expression (after @?@, before @:@)
            Annotations       -- ^ succeeding @:@ symbol
            Expr              -- ^ Right-hand side (after @:@)
          | Binary            -- ^ Binary operation
            Expr              -- ^ Left-hand side
            BinaryOperator    -- ^ binary operator
            Annotations       -- ^ succeeding the binary operator token
            Expr              -- ^ Right-hand side
          | LogicalNeg        -- ^ Logical negation (@!@)
            Annotations       -- ^ succeeding @!@ token
            Expr              -- ^ Right-hand side
          | UnaryPlus         -- ^ Promotion (prefix @+@), no-op
            Annotations       -- ^ succeeding @+@ token
            Expr              -- ^ Right-hand side
          | UnaryMinus        -- ^ Arithmetic negation (prefix @-@)
            Annotations       -- ^ succeeding @-@ token
            Expr              -- ^ Right-hand side
          | Transpose         -- ^ Matrix transposition
            Expr              -- ^ expression to be transposed
            Annotations       -- ^ succeeding @'@ token
          | Call              -- ^ Function application
            Expr              -- ^ callee
            [Expr]            -- ^ arguments
            [Annotations]     -- ^ annotations preceding each argement
            Annotations       -- ^ succeeding closing @)@ token
          | CallBar           -- ^ Function application @f(a | ...)@
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
            (Maybe Expr)      -- ^ optional expression to be returned
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
          | IncrementLogProb  -- ^ increment log prob (@increment_log_prob(..)@)
            Annotations       -- ^ succeeding the @increment_log_prob@ keyword
            Annotations       -- ^ succeeding the opening @(@ symbol
            Expr              -- ^ parameter
            Annotations       -- ^ succeeding the closing @)@ symbol
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
          | Empty             -- ^ empty statement, i.e. @;@
            Annotations       -- ^ succeeding the @;@ symbol
          | VarDecl            -- ^ declaration without assignment
            VarType            -- ^ type
            Expr               -- ^ identifier
            (Maybe ArrayDims)  -- ^ optional array dimensions
            Annotations        -- ^ after @;@ symbol
          | VarDeclAssign      -- ^ declaration with assignment
            VarType            -- ^ type
            Expr               -- ^ identifier
            (Maybe ArrayDims)  -- ^ optional array dimensions
            Annotations        -- ^ after @=@ symbol
            Expr               -- ^ right-hand side of assignment
            Annotations        -- ^ after @;@ symbol
          deriving (Eq, Show)


-- | Binary operator
data BinaryOperator = Or                -- ^ Logical "or" disjunction (@!!@)
                    | And               -- ^ Logical "and" conjunction (@&&@)
                    | Equal             -- ^ Equal comparison (@==@)
                    | NotEqual          -- ^ Not-equal comparison (@!=@)
                    | Gt                -- ^ Greater than (@>@)
                    | Geq               -- ^ Greater or equal than (@>=@)
                    | Lt                -- ^ Less than (@<@)
                    | Leq               -- ^ Less or equal than (@<=@)
                    | Add               -- ^ Addition (@+@)
                    | Sub               -- ^ Subtraction (@-@)
                    | Mul               -- ^ Multiplication (@*@)
                    | Div               -- ^ Division (@/@)
                    | EltMul            -- ^ Element-wise multiplication (@.*@)
                    | EltDiv            -- ^ Element-wise division (@./@)
                    | LDiv              -- ^ Left-division (@\\@)
                    | IntDiv            -- ^ Integer division (@%\\%@)
                    | Pow               -- ^ Exponentiation (@^@)
                    | EltPow            -- ^ Element-wise exponentiation (@.^@)
                    deriving (Eq, Show)


-- | top var type
data VarType = Int                           -- ^ integer
               Annotations                   -- ^ after @int@
               (Maybe VarConstraints)        -- ^ constraints
             | Real                          -- ^ real number
               Annotations                   -- ^ after @real@
               (Maybe VarConstraints)        -- ^ constraints
             | Complex                       -- ^ complex number
               Annotations                   -- ^ after @complex@
               (Maybe VarConstraints)        -- ^ constraints
             | Vector                        -- ^ real vector
               Annotations                   -- ^ after @vector@
               (Maybe VarConstraints)        -- ^ constraints
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | RowVector                     -- ^ row vector
               Annotations                   -- ^ after @row_vector@
               (Maybe VarConstraints)        -- ^ constraints
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | Matrix                        -- ^ real matrix
               Annotations                   -- ^ after @matrix@
               (Maybe VarConstraints)        -- ^ constraints
               Annotations                   -- ^ after @[@
               Expr                          -- ^ first dimensions
               Annotations                   -- ^ after @,@
               Expr                          -- ^ second dimensions
               Annotations                   -- ^ after @]@
             | ComplexVector                 -- ^ complex vector
               Annotations                   -- ^ after @complex_vector@
               (Maybe VarConstraints)        -- ^ constraints
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | ComplexRowVector              -- ^ complex row vector
               Annotations                   -- ^ after @complex_row_vector@
               (Maybe VarConstraints)        -- ^ constraints
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | ComplexMatrix                 -- ^ complex real matrix
               Annotations                   -- ^ after @complex_matrix@
               (Maybe VarConstraints)        -- ^ constraints
               Annotations                   -- ^ after @[@
               Expr                          -- ^ first dimensions
               Annotations                   -- ^ after @,@
               Expr                          -- ^ second dimensions
               Annotations                   -- ^ after @]@
             | Ordered                       -- ^ ordered real vector
               Annotations                   -- ^ after @ordered@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | PositiveOrdered               -- ^ positive ordered vector
               Annotations                   -- ^ after @positive_ordered@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | Simplex                       -- ^ element in simplex
               Annotations                   -- ^ after @simplex@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | UnitVector                    -- ^ unit vector
               Annotations                   -- ^ after @unit_vector@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | CholeskyFactorCorr            -- ^ cholesky factor correlation
               Annotations                   -- ^ after @cholesky_factor_corr@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | CholeskyFactorCov             -- ^ cholesky factor covariance
               Annotations                   -- ^ after @cholesky_factor_cov@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               (Maybe (Annotations, Expr))   -- ^ optional: annotations after
                                             --   @,@ and second dimensions
               Annotations                   -- ^ after @]@
             | CorrMatrix                    -- ^ correlation matrix
               Annotations                   -- ^ after @corr_matrix@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             | CovMatrix                     -- ^ covariance matrix
               Annotations                   -- ^ after @cov_matrix@
               Annotations                   -- ^ after @[@
               Expr                          -- ^ dimensions
               Annotations                   -- ^ after @]@
             deriving (Eq, Show)


-- | type synonym for constructors of `VarType` scalar types
type ScalarVarType = Annotations -> Maybe VarConstraints -> VarType

-- | type synonym for constructors of `VarType` vector types
type VectorVarType = Annotations
                      -> Maybe VarConstraints
                      -> Annotations
                      -> Expr
                      -> Annotations
                      -> VarType

-- | type synonym for constructors of `VarType` vector types that must not
--   have constraints
type VectorNoConstrVarType = Annotations
                              -> Annotations
                              -> Expr
                              -> Annotations
                              -> VarType

-- | type synonym for constructors of `VarType` matrix types
type MatrixVarType = Annotations
                      -> Maybe VarConstraints
                      -> Annotations
                      -> Expr
                      -> Annotations
                      -> Expr
                      -> Annotations
                      -> VarType

-- | type synonym for constructors of `VarType` matrix types
type MatrixOptDimsVarType = Annotations
                             -> Annotations
                             -> Expr
                             -> Maybe (Annotations, Expr)
                             -> Annotations
                             -> VarType

-- | set of variable constraints
data VarConstraints = VarConstraints
                      [VarConstraint]
                      Annotations     -- ^ after the @>@ symbol
                    deriving (Eq, Show)

-- | single variable constraint
data VarConstraint = Lower
                     Annotations  -- ^ before @lower@ keyword
                     Annotations  -- ^ after @lower@ keyword
                     Annotations  -- ^ after @=@ symbol
                     Expr       -- ^ lower bound
                   | Upper
                     Annotations  -- ^ before @upper@ keyword
                     Annotations  -- ^ after @upper@ keyword
                     Annotations  -- ^ after @=@ symbol
                     Expr       -- ^ upper bound
                   | Multiplier
                     Annotations  -- ^ before @multiplier@ keyword
                     Annotations  -- ^ after @multiplier@ keyword
                     Annotations  -- ^ after @=@ symbol
                     Expr  -- ^ multiplier
                   | Offset
                     Annotations  -- ^ before @offset@ keyword
                     Annotations  -- ^ after @offset@ keyword
                     Annotations  -- ^ after @=@ symbol
                     Expr      -- ^ offset
                   deriving (Eq, Show)


-- | Array dimensions
data ArrayDims = ArrayDims
                 [(Annotations, Expr)]  -- ^ annotations and dimensions
                 Annotations            -- ^ annotations after @]@
               deriving (Eq, Show)


-- | Annotations of source code (comments, linebreaks)
data CodeAnnotation = LineBased String   -- ^ @// ...@ style comment
                    | Bracketed String   -- ^ @//* ... *//@ style comment
                    | Newline            -- ^ Line break
                    deriving (Eq, Show)

-- | Type synonym, zero or more code annotations
type Annotations = [CodeAnnotation]
