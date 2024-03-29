{-# LANGUAGE OverloadedStrings #-}
module Text.Nast.Parser (
  -- * stan program
    stanProgram
  -- * literals
  , numLiteral
  , signedInt
  , stringLiteral
  -- * expressions
  , expression
  , constrExpression
  , printables
  , precedence10
  , precedence9
  , precedence8
  , precedence7
  , precedence6
  , precedence5
  , precedence4
  , precedence3
  , precedence2
  , precedence1
  , precedence0
  , completeCall
  , completeBarCall
  , completeIndex
  , CallLikeConstr
  , callLike
  , range
  , lhs
  , primary
  , identifier
  , parentheses
  -- * code annotations
  , codeAnnotations
  , codeAnnotations1
  , achar
  , astring
  , astringsTuple
  , comment
  , bracketed
  , lineBased
  -- * shared
  , eol
  , newline
  , whitespace
  , whitespace1
  -- * statements
  , statement
  , keyword
  , block
  , ifElse
  , for
  , while
  , tilde
  , printStmt
  , reject
  , targetPlusAssign
  , incrementLogProb
  , emptyStmt
  -- * declarations and var types
  , AllowVarConstraints
  , AllowAssignment
  , varDeclaration
  , varType
  , varConstraint
  , varConstraints
  , scalarVarType
  , vectorVarType
  , matrixVarType
  , matrixOptDimsVarType
  , arrayDims
  ) where


import Text.Parsec.Text (Parser)
import Text.Parsec
  ( (<|>)
  , (<|>)
  , (<?>)
  , anyChar
  , chainl1
  , char
  , digit
  , eof
  , letter
  , lookAhead
  , many
  , many1
  , manyTill
  , noneOf
  , oneOf
  , optionMaybe
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , string
  , try
  )
import Control.Monad (void)
import Data.Maybe (maybeToList)

import Text.Nast.AnnotatedAST


-- | Stan program
stanProgram :: Parser StanProgram
stanProgram =
  do h   <- codeAnnotations
     let fb = Nothing  -- TODO not implemented
     db  <- optionMaybe $ programBlock "data"                   True False False
     tdb <- optionMaybe $ programBlock "transformed data"       True True True
     pb  <- optionMaybe $ programBlock "parameters"             True False False
     tpb <- optionMaybe $ programBlock "transformed parameters" True True True
     mb  <- optionMaybe $ programBlock "model"                  False True True
     gqb <- optionMaybe $ programBlock "generated quantities"   True True True
     _ <- eol
     return $ StanProgram h fb db tdb pb tpb mb gqb

-- | allow variable constraints in declaration
type AllowVarConstraints = Bool

-- | allow initial assignment in the right-hand side of declaration
type AllowAssignment = Bool

-- | allow statements other than variable declarations
type AllowStatements = Bool

-- | Stan program block
programBlock :: String               -- ^ block name
             -> AllowVarConstraints  -- ^ allow var constraints
             -> AllowAssignment      -- ^ allow initial assignments
             -> AllowStatements      -- ^ allow non-declaration statements
             -> Parser ProgramBlock
programBlock name allowConstr allowAssign allowStmts =
  do xs <- try $ astring name
     ys <- achar '{'
     stmts <- many stmt
     zs <- achar '}'
     return $ ProgramBlock xs ys stmts zs
  where stmt = if allowStmts
                 then declarations <|> statement
                 else declarations
        declarations = varDeclaration allowConstr allowAssign


-- | Stan numeric literal
numLiteral :: Parser Expr
numLiteral = do i <- signedInt
                d <- optionMaybe (char '.' >> many1 digit)
                e <- optionMaybe (char 'e' >> signedInt)
                xs <- codeAnnotations
                return $ NumLiteral i d e xs

-- | Signed integer number, e.g., @+3@, @-8@, @2@
signedInt :: Parser String
signedInt = do plus <|> minus <|> nosign
    where nosign = many1 digit
          plus = (:) <$> char '+' <*> nosign
          minus = (:) <$> char '-' <*> nosign

-- | String literal, e.g. @"abc"@
stringLiteral :: Parser Expr
stringLiteral = do s <- char '"' >> (many $ noneOf "\n\"")
                   xs <- achar '"'
                   return $ StringLiteral s xs

-- | Stan expression
expression :: Parser Expr
expression = precedence10

-- | Constrained expression, as used for type constraints
constrExpression :: Parser Expr
constrExpression = precedence5

-- | List of printables. Used in @print@ and @reject@ statements.
printables :: Parser Expr
printables = do (as, es) <- unzip <$> p `sepBy1` char ','
                return $ uncurry Printables (es, as)
  where p = (,) <$> codeAnnotations <*> (stringLiteral <|> expression)

{-|
Precedence level 10 expressions

* Ternary @?:@ op, ternary infix, right associative;

Here, right associative means that @a ? b : c ? d : e@ is equivalent to
@a ? b : (c ? d : e)@.
-}
precedence10 :: Parser Expr
-- Here we try to ensure that the precedence9 expression is only parsed once.
-- For a parser of the form @try conditional <|> precedence9@ this might happen
-- twice if there is in fact no ternary @?:@ operation. However, this is
-- unnecessarily expensive if the precedence9 expression is complex.
precedence10 = do l <- precedence9
                  p l <|> return l
  where p l' = do xs <- achar '?'
                  m <- precedence9
                  ys <- achar ':'
                  r <- precedence10
                  return $ Conditional l' xs m ys r

{-|
Precedence level 9 expressions

* logical @||@ op, binary infix, left associative; "or" disjunction
-}
precedence9 :: Parser Expr
precedence9 = chainl1 precedence8 $ binary "||" Or

{-|
Precedence level 8 expressions

* logical @&&@ op, binary infix, left associative; "and" conjunction
-}
precedence8 :: Parser Expr
precedence8 = chainl1 precedence7 $ binary "&&" And

{-|
Precedence level 7 expressions

* logical @==@ op, binary infix, left associative; equal to
* logical @!=@ op, binary infix, left associative; not equal to
-}
precedence7 :: Parser Expr
precedence7 = chainl1 precedence6 $ binary "!=" NotEqual <|> binary "==" Equal

{-|
Precedence level 6 expressions

* @<@  op, binary infix, left associative; smaller than
* @<=@ op, binary infix, left associative; smaller or equal than
* @>@  op, binary infix, left associative; greater than
* @>=@ op, binary infix, left associative; greater or equal than
-}
precedence6 :: Parser Expr
precedence6 = chainl1 precedence5 op
  where op = try (binary "<=" Leq)
             <|> binary "<" Lt
             <|> try (binary ">=" Geq)
             <|> binary ">" Gt

{-|
Precedence level 5 expressions

* @+@ op, binary infix, left associative; addition
* @-@ op, binary infix, left associative; subtraction
-}
precedence5 :: Parser Expr
precedence5 = chainl1 precedence4 $ binary "+" Add <|> binary "-" Sub

{-|
Precedence level 4 expressions

* @*@ op, binary infix, left associative; multiplication
* @.*@ op, binary infix, left associative; element-wise multiplication
* @/@ op, binary infix, left associative; division
* @./@ op, binary infix, left associative; element-wise division
* @%@ op, binary infix, left associative; modulo
-}
precedence4 :: Parser Expr
precedence4 = chainl1 precedence3 op
  where op = binary "*" Mul
             <|> binary "/" Div
             <|> try (binary ".*" EltMul)
             <|> binary "./" EltDiv

{-|
Precedence level 3 expressions

* @\@ op, binary infix, left associative; left division
* @%\%@ op, binary infix, left associative; integer division
-}
precedence3 :: Parser Expr
-- precedence3 = chainl1 precedence2 $ binary ["\\", "%\\%"]
precedence3 = chainl1 precedence2 $ binary "\\" LDiv <|> binary "%\\%" IntDiv

{-|
Precedence level 2 expressions

* @!@ op, unary prefix; logical negation
* @+@ op, unary prefix; promotion (no-op)
* @-@ op, unary prefix; negation
-}
precedence2 :: Parser Expr
precedence2 = lneg <|> plus <|> minus <|> precedence1
  where lneg  = char '!' >> LogicalNeg <$> codeAnnotations <*> precedence2
        plus  = char '+' >> UnaryPlus <$> codeAnnotations <*> precedence2
        minus = char '-' >> UnaryMinus <$> codeAnnotations <*> precedence2

{-|
Precedence level 1 expressions

* @^@ op, binary infix, right associative; exponentiation
* @.^@ op, binary infix, right associative; element-wise exponentiation
-}
precedence1 :: Parser Expr
precedence1 = do e <- precedence0
                 (try $ char '^' >> completeBin Pow e) <|>
                   (try $ string ".^" >> completeBin EltPow e) <|>
                   return e
  where completeBin op e = do xs <- codeAnnotations
                              r <- precedence1
                              return $ Binary e op xs r

{-|
Precedence level 0 expressions

* @'@ op, postfix; matrix transposition
* @()@ function application
* @[]@ array & matrix indexing
-}
precedence0 :: Parser Expr
precedence0 = do e <- primary  -- Parse primary only once for efficiency
                 (t e) <|> completeBarCall e
                       <|> completeCall e
                       <|> completeIndex e
                       <|> return e
  where t e = do xs <- achar '\''
                 return $ Transpose e xs

{-|
Complete function call. We assume that the call is already consumed and passed
as an argument.
-}
completeCall :: Expr -> Parser Expr
completeCall = callLike '(' ')' Call expression

{-|
Complete function call of the form @f(a | b, ..)@. We assume that the call is
already consumed and passed as an argument.
-}
completeBarCall :: Expr -> Parser Expr
completeBarCall callee =
  do (xs, e) <- try $ (,) <$> achar '(' <*> expression <* char '|'
     tuples <- ((,) <$> codeAnnotations <*> expression) `sepBy` (char ',')
     let (as, args) = if null tuples then ([[]], []) else unzip tuples
     ys <- achar ')'
     return $ CallBar callee (e:args) (xs:as) ys

{-|
Complete array/vector/matrix expression. We assume that the object to be indexed
is already consumed and passed as an argument.
-}
completeIndex :: Expr -> Parser Expr
completeIndex e = do e' <- callLike '[' ']' Index range e
                     (completeIndex e' <|> return e')

{-| Type synonym for type of 'Call' and 'Index' constructors -}
type CallLikeConstr = Expr
                    -> [Expr]
                    -> [Annotations]
                    -> Annotations
                    -> Expr

{-|
Complete "call-like" expressions, i.e. function applications and array indexing.
Here, it is assumed that the identifier (callee or array to be indexed) has
already been consumed and is passed as an argument (see below).
-}
callLike :: Char                           -- ^ Left delimiter, @(@ or @[@
         -> Char                           -- ^ Right delimiter, @)@ or @]@
         {-| Constructor for result, i.e., @Call@ or @Range@. -}
         -> CallLikeConstr
         {-| Parser for parsing a single argument or index -}
         -> (Parser Expr)
         -> Expr                           -- ^ Identifier (callee).
         -> Parser Expr
callLike ld rd f p e = do _ <- char ld
                          (argAnns, args) <- (try withArgs) <|> withoutArgs
                          cs <- achar rd
                          return $ f e args argAnns cs
  where -- first case: if there is at least one argument
        -- withArgs = do todo <- q `sepBy` (char ',')
        withArgs = do xs <- ((,) <$> codeAnnotations <*> p) `sepBy` (char ',')
                      -- if there are no arguments, 'as' should be [[]] not []
                      -- for consistency.
                      let (as, args) = if null xs then ([[]], []) else unzip xs
                      return $ (as, args)
        -- second case: if there are no arguments there can still be code
        -- code annotations, e.g. @f( /* abc */ )@.
        withoutArgs = do as <- codeAnnotations
                         return $ ([as], [])

{-|
Range

Range expression as used in array indexing or for loop ranges. Examples:
@1:10@, @:3@, @1:@, or @:@.
-}
range :: Parser Expr
range = do l <- optionMaybe $ try expression
           colon <- optionMaybe $ char ':'
           case (l, colon) of
             (Just e,  Nothing) -> return e
             (Nothing, Nothing) -> fail "Cannot parse range"
             _                  -> do cs <- codeAnnotations
                                      r <- optionMaybe $ try expression
                                      return $ Range l cs r

{-| Left-hand side expression

Expressions that are allowed on the left-hand side of assignments are
identifiers with zero or more array/matrix indices, such as @idfr@, @idfr[3]@,
@idfr[3][5]@ and so on
-}
lhs :: Parser Expr
lhs = do i <- identifier
         (completeIndex i) <|> (return i)

primary :: Parser Expr
primary = numLiteral <|> parentheses <|> identifier

{-|
Identifier

The following constraints must hold:
* Only @a-z@, @A-Z@, @_@, @0-9@ allowed.
* Must start with letter.
* Must not end with two underscores: @__@.
-}
identifier :: Parser Expr
identifier = do x <- letter
                xs <- many $ letter <|> digit <|> char '_'
                let p = take 2 . reverse $ xs
                ys <- codeAnnotations
                if p == "__"
                  then fail "identifier must not end with '__'"
                  else return $ Identifier (x:xs) ys

parentheses :: Parser Expr
parentheses = do xs <- achar '('
                 e <- expression
                 ys <- achar ')'
                 return $ Parens xs e ys

{-| Parse annotated binary operator on expressions -}
binary :: String -> BinaryOperator -> Parser (Expr -> Expr -> Expr)
binary s op = do xs <- (try $ string s) >> codeAnnotations
                 return $ (\l r -> Binary l op xs r)

-- | Zero or more comments and newlines
codeAnnotations :: Parser Annotations
codeAnnotations = whitespace >> (newline <|> comment) `sepEndBy` whitespace

-- | One or more comments and newlines
codeAnnotations1 :: Parser Annotations
codeAnnotations1 = try annotations <|> justWhitespaces
  where annotations = whitespace >> (newline <|> comment) `sepEndBy1` whitespace
        justWhitespaces = whitespace1 >> return []

-- | Parse annotated char: consume char then parse annotations
achar :: Char -> Parser Annotations
achar c = char c >> codeAnnotations

-- | Consume string then parse annotations
astring :: String -> Parser Annotations
astring s = string s >> codeAnnotations

-- | Parse two strings each followed by annotations
astringsTuple :: String -> String -> Parser (Annotations, Annotations)
astringsTuple a b = (,) <$> astring a <*> astring b

-- | Stan comment
comment :: Parser CodeAnnotation
comment = lineBased <|> bracketed <?> "comment"

-- | @//* ... *//@ style comment
bracketed :: Parser CodeAnnotation
bracketed = do c <- (try $ string "/*") >> manyTill anyChar (try $ string "*/")
               return $ Bracketed c

-- | @// ...@ style comment
lineBased :: Parser CodeAnnotation
lineBased = do c <- (try $ string "//") >> manyTill (noneOf "\n") eol
               return $ LineBased c

eol :: Parser ()
eol = (lookAhead eof) <|> (void $ char '\n') <?> "end of line"

newline :: Parser CodeAnnotation
newline = char '\n' >> return Newline <?> "newline"

-- | Zero or more space or tab characters
whitespace :: Parser String
whitespace = many $ oneOf " \t"

-- | One or more space or tab characters
whitespace1 :: Parser String
whitespace1 = many1 $ oneOf " \t"

-- | Parse statements
statement :: Parser Stmt
statement =   (try $ keyword "break" Break)
          <|> (try $ keyword "continue" Continue)
          <|> try returnStmt
          <|> printStmt
          <|> reject
          <|> block
          <|> ifElse
          <|> for
          <|> while
          <|> targetPlusAssign
          <|> incrementLogProb
          <|> assignment
          <|> tilde
          <|> emptyStmt
          <?> "statement"

-- | Parse keyword statement such as @break@ or @continue@
keyword :: String                                -- ^ keyword name
        -> (Annotations -> Annotations -> Stmt)  -- ^ constructor for Stmt
        -> Parser Stmt
keyword k constr = do xs <- astring k
                      ys <- achar ';'
                      return $ constr xs ys

-- | Parse block statement
block :: Parser Stmt
block = do xs <- achar '{'
           stmts <- many statement
           ys <- achar '}'
           return $ Block xs stmts ys

-- | Parse if then (else) statements
ifElse :: Parser Stmt
ifElse = do (xs, ys) <- try $ astringsTuple "if" "("
            cond <- expression
            zs <- achar ')'
            stmt <- statement
            elseAnn <- optionMaybe $ try $ astring "else"
            case elseAnn of
              Nothing -> return $ If xs ys cond zs stmt
              Just vs -> do stmt' <- statement
                            return $ IfElse xs ys cond zs stmt vs stmt'

-- | Parse assignment statement ops such as @=@, @<-@, @+=@, @*=@
assignment :: Parser Stmt
assignment = do (l, (xs, op)) <- try $ (,) <$> lhs <*> assignOp
                r <- expression
                ys <- achar ';'
                return $ op l xs r ys

{-| Type synonym for construrtors assignment (Assign, PlusAssign, ...) nodes -}
type AssignConstr = Expr          -- ^ left-hand side
                  -> Annotations  -- ^ annotations after op token
                  -> Expr         -- ^ right-hand side
                  -> Annotations  -- ^ annotations after ; token
                  -> Stmt

{-| Parse assignment operator and return annotations & respective constructor -}
assignOp :: Parser ([CodeAnnotation], AssignConstr)
assignOp = foldr1 (<|>) [p s f | (s, f) <- tokenConsts]
  where p t g = try $ (,) <$> (astring t) <*> return g
        tokenConsts = [ ("=", Assign)
                      , ("<-", ArrowAssign)
                      , ("+=", PlusAssign)
                      , ("-=", MinusAssign)
                      , ("*=", TimesAssign)
                      , ("/=", DivideAssign)
                      , (".*=", EltTimesAssign)
                      , ("./=", EltDivideAssign)
                      ]

{-| Parse for-loop statement -}
for :: Parser Stmt
for = do (xs, ys) <- try $ astringsTuple "for" "("
         i <- identifier
         zs <- astring "in"
         l <- expression
         opt_r <- optionMaybe $ try $
                    char ':' >> (,) <$> codeAnnotations <*> expression
         vs <- achar ')'
         b <- statement
         case opt_r of
           (Just (ws, r)) -> return $ ForRange xs ys i zs l ws r vs b
           _ -> return $ For xs ys i zs l vs b

{-| Parse while-loop statement -}
while :: Parser Stmt
while = do (xs, ys) <- try $ astringsTuple "while" "("
           cond <- expression
           zs <- achar ')'
           b <- statement
           return $ While xs ys cond zs b

{-| Tilde statement -}
tilde :: Parser Stmt
tilde = do (l, xs) <- try $ (,) <$> expression <*> (achar '~')
           i <- identifier
           r <- completeCall i
           ys <- achar ';'
           return $ Tilde l xs r ys

{-| Return statement -}
returnStmt :: Parser Stmt
returnStmt = do xs <- astring "return"
                e <- optionMaybe $ try expression
                ys <- achar ';'
                return $ Return xs e ys

{-| Print statement -}
printStmt :: Parser Stmt
printStmt = do xs <- try $ astring "print" <* char '('
               ps <- printables
               ys <- achar ')'
               zs <- achar ';'
               return $ Print xs ps ys zs

{-| Reject statement -}
reject :: Parser Stmt
reject = do xs <- try $ astring "reject" <* char '('
            ps <- printables
            ys <- achar ')'
            zs <- achar ';'
            return $ Reject xs ps ys zs

{-| Target plus-assign statement, e.g., @target += expr;@. -}
targetPlusAssign :: Parser Stmt
targetPlusAssign = do (xs, ys) <- try $ astringsTuple "target" "+="
                      e <- expression
                      zs <- achar ';'
                      return $ TargetPlusAssign xs ys e zs

{-| Increment log prob call -}
incrementLogProb :: Parser Stmt
incrementLogProb = do (xs, ys) <- try $
                        astringsTuple "increment_log_prob" "("
                      e <- expression
                      zs <- achar ')'
                      vs <- achar ';'
                      return $ IncrementLogProb xs ys e zs vs

emptyStmt :: Parser Stmt
emptyStmt = achar ';' >>= return . Empty

-- | top var declaration
varDeclaration :: AllowVarConstraints  -- ^ allow var constraints
               -> AllowAssignment      -- ^ allow initial assignments
               -> Parser Stmt
varDeclaration allowConstraints allowAssignment =
  do t <- try $ varType allowConstraints
     i <- identifier
     dims <- optionMaybe $ try arrayDims
     if allowAssignment
       then do optAssign <- try $ optionMaybe assign
               ys <- achar ';'
               case optAssign of
                 (Just (xs, rhs)) -> return $ VarDeclAssign t i dims xs rhs ys
                 _ -> return $ VarDecl t i dims ys
       else do ys <- achar ';'
               return $ VarDecl t i dims ys
  where assign =  do xs <- achar '='
                     rhs <- expression
                     return (xs, rhs)

-- | top var types
varType :: AllowVarConstraints -> Parser VarType
varType a = foldr1 (<|>) (map try ps)
  where
    ps = [ scalarVarType "int" a Int
         , scalarVarType "real" a Real
         , scalarVarType "complex" a Complex
         , vectorVarType "vector" a Vector
         , vectorVarType "row_vector" a RowVector
         , matrixVarType "matrix" a Matrix
         , vectorVarType "complex_vector" a ComplexVector
         , vectorVarType "complex_row_vector" a ComplexRowVector
         , matrixVarType "complex_matrix" a ComplexMatrix
         , vectorNoConstrVarType "ordered" Ordered
         , vectorNoConstrVarType "positive_ordered" PositiveOrdered
         , vectorNoConstrVarType "simplex" Simplex
         , vectorNoConstrVarType "unit_vector" UnitVector
         , vectorNoConstrVarType "cholesky_factor_corr" CholeskyFactorCorr
         , matrixOptDimsVarType "cholesky_factor_cov" CholeskyFactorCov
         , vectorNoConstrVarType "corr_matrix" CorrMatrix
         , vectorNoConstrVarType "cov_matrix" CovMatrix
         ]

-- | variable constraint (upper/lower bounds, multiplier/offset)
varConstraint :: Parser VarConstraint
varConstraint = foldr1 (<|>) (map try ps)
  where ps = [ p "lower" Lower
             , p "upper" Upper
             , p "multiplier" Multiplier
             , p "offset" Offset
             ]
        p s f = do xs <- codeAnnotations
                   ys <- astring s
                   zs <- achar '='
                   e <- constrExpression
                   return $ f xs ys zs e

-- | one or two variable constraints, e.g. @<lower=0,upper=1>@
varConstraints :: Parser VarConstraints
varConstraints = do _ <- char '<'
                    vcl <- varConstraint
                    vcr <- optionMaybe $ char ',' >> varConstraint
                    let vcs = vcl : (maybeToList vcr)
                    ys <- achar '>'
                    return $ VarConstraints vcs ys

-- | create parser for scalar `VarType`s
scalarVarType :: String                  -- ^ type name
                 -> AllowVarConstraints  -- ^ allow var constraints
                 -> ScalarVarType        -- ^ scalar `VarType` constructor
                 -> Parser VarType
scalarVarType s a f = do xs <- astring s
                         vc <- if a
                                then optionMaybe varConstraints
                                else return Nothing
                         return $ f xs vc

-- | create parser for vector `VarType`s
vectorVarType :: String               -- ^ type name
              -> AllowVarConstraints  -- ^ allow var constraints
              -> VectorVarType        -- ^ vector `VarType` constructor
              -> Parser VarType
vectorVarType s a f = do xs <- astring s
                         vc <- if a
                                then optionMaybe varConstraints
                                else return Nothing
                         ys <- achar '['
                         d <- expression
                         zs <- achar ']'
                         return $ f xs vc ys d zs

-- | create parser for vector `VarType`s that must not have constraints
vectorNoConstrVarType :: String
                      -> VectorNoConstrVarType
                      -> Parser VarType
vectorNoConstrVarType s f = do xs <- astring s
                               ys <- achar '['
                               d <- expression
                               zs <- achar ']'
                               return $ f xs ys d zs

-- | create parser for matrix `VarType`s
matrixVarType :: String                  -- ^ type name
                 -> AllowVarConstraints  -- ^ allow var constraints
                 -> MatrixVarType        -- ^ vector `VarType` constructor
                 -> Parser VarType
matrixVarType s a f = do xs <- astring s
                         vc <- if a
                                then optionMaybe varConstraints
                                else return Nothing
                         ys <- achar '['
                         d1 <- expression
                         zs <- achar ','
                         d2 <- expression
                         vs <- achar ']'
                         return $ f xs vc ys d1 zs d2 vs

-- | create parser for matrix `VarType`s with optional second dimensions
matrixOptDimsVarType :: String
                     -> MatrixOptDimsVarType
                     -> Parser VarType
matrixOptDimsVarType s f = do xs <- astring s
                              ys <- achar '['
                              d1 <- expression
                              ad2 <- optionMaybe $
                                      (,) <$> (achar ',')
                                          <*> expression
                              zs <- achar ']'
                              return $ f xs ys d1 ad2 zs

-- | array dimensions
arrayDims :: Parser ArrayDims
arrayDims = do _ <- char '['
               ds <- p `sepBy` (char ',')
               xs <- achar ']'
               return $ ArrayDims ds xs
  where p = (,) <$> codeAnnotations <*> expression
