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
                   deriving (Eq, Show)

-- | Annotations of source code (comments, linebreaks)
data CodeAnnotation = LineBased String   -- ^ @// ...@ style comment
                    | Bracketed String   -- ^ @//* ... *//@ style comment
                    | Newline            -- ^ Line break
                    deriving (Eq, Show)
