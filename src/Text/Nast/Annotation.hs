module Text.Nast.Annotation
  ( ASTAnnotation (..)
  , CodeAnnotation (..)
  ) where


-- | Annotation for parsed AST
data ASTAnnotation = PrimaryAnn        -- ^ Annotations of primary expressions
                     [CodeAnnotation]  -- ^ Annotations after the primary
                                       -- ^ expression
                   | BinaryAnn         -- ^ Annotation of binary infix operations
                     [CodeAnnotation]  -- ^ Annotations after the binary op token
                   | UnaryAnn          -- ^ Annotations of unary expressions
                     [CodeAnnotation]  -- ^ Annotations after (if prefix op) or
                                       -- ^ before (if postfix op) the op token
                   | ParensAnn         -- ^ Annotations for parenthesized
                                       -- ^ expressions
                     [CodeAnnotation]  -- ^ Annotations after opening @(@
                     [CodeAnnotation]  -- ^ Annotations after closing @)@
                   | CondAnn           -- ^ Annotation for ternary @?:@ op
                     [CodeAnnotation]  -- ^ Annotations after @?@ token
                     [CodeAnnotation]  -- ^ Annotations after the @:@ token
                   deriving (Eq, Show)

-- | Annotations of source code (comments, linebreaks)
data CodeAnnotation = LineBased String   -- ^ @// ...@ style comment
                    | Bracketed String   -- ^ @//* ... *//@ style comment
                    | Newline            -- ^ Line break
                    deriving (Eq, Show)
