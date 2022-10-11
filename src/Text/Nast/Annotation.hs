module Text.Nast.Annotation
  ( ASTAnnotation (..)
  , CodeAnnotation (..)
  ) where


-- | Annotation for parsed AST
data ASTAnnotation = PrimaryAnn          -- ^ Primary expressions
                     [CodeAnnotation]    -- ^ succeeding the primary expression
                   | BinaryAnn           -- ^ Binary infix operations
                     [CodeAnnotation]    -- ^ succeeding the binary operator
                   | UnaryAnn            -- ^ Unary expressions
                     [CodeAnnotation]    -- ^ succeeding (if prefix) or
                                         --   preceding (if postfix) operator
                   | ParensAnn           -- ^ Parentheseses
                     [CodeAnnotation]    -- ^ succeeding opening @(@
                     [CodeAnnotation]    -- ^ succeeding closing @)@
                   | CondAnn             -- ^ Ternary expressions
                     [CodeAnnotation]    -- ^ succeeding the first op token
                     [CodeAnnotation]    -- ^ succeeding the second op token
                   | CallAnn             -- ^ Function application
                     [[CodeAnnotation]]  -- ^ succeeding the @(@ and commas
                     [CodeAnnotation]    -- ^ succeeding closing @)@
                   | KeywordAnn          -- ^ Keyword statements like @break@
                     [CodeAnnotation]    -- ^ succeeding the keyword
                     [CodeAnnotation]    -- ^ succeeding the semicolon
                   | BlockAnn            -- ^ Block statements (@{..}@)
                     [CodeAnnotation]    -- ^ succeeding opening @{@
                     [CodeAnnotation]    -- ^ succeeding closing @}@
                   | IfAnn               -- ^ If / else statement
                     [CodeAnnotation]    -- ^ succeeding @if@ keyword
                   | IfElseAnn           -- ^ If / else statement
                     [CodeAnnotation]    -- ^ succeeding @if@ keyword
                     [CodeAnnotation]    -- ^ succeeding @else@ keyword
                   | AssignAnn           -- ^ assignment statement
                     [CodeAnnotation]    -- ^ succeeding the assignment op token
                     [CodeAnnotation]    -- ^ succeeding the closing @;@
                   | ForAnn              -- ^ for loop without range
                     [CodeAnnotation]    -- ^ succeeding @for@ keyword
                     [CodeAnnotation]    -- ^ succeeding opening @(@
                     [CodeAnnotation]    -- ^ succeeding @in@ keyword
                     [CodeAnnotation]    -- ^ succeeding closing @)@
                   | ForRangeAnn         -- ^ for loop without range
                     [CodeAnnotation]    -- ^ succeeding @for@ keyword
                     [CodeAnnotation]    -- ^ succeeding opening @(@
                     [CodeAnnotation]    -- ^ succeeding @in@ keyword
                     [CodeAnnotation]    -- ^ succeeding @:@ symbol
                     [CodeAnnotation]    -- ^ succeeding closing @)@
                   | WhileAnn            -- ^ while loop
                     [CodeAnnotation]    -- ^ succeeding the @while@ keyword
                   deriving (Eq, Show)

-- | Annotations of source code (comments, linebreaks)
data CodeAnnotation = LineBased String   -- ^ @// ...@ style comment
                    | Bracketed String   -- ^ @//* ... *//@ style comment
                    | Newline            -- ^ Line break
                    deriving (Eq, Show)
