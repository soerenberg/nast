module Text.Nast.Parser (
  -- * literals
    numLiteral
  , signedInt
  , stringLiteral
  -- * expressions
  , expression
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
  , completeIndex
  , CallLikeConstr
  , callLike
  , range
  , lhs
  , primary
  , identifier
  , parentheses
  , BinConstr
  , binOp
  -- * code annotations
  , codeAnnotations
  , codeAnnotations1
  , comment
  , bracketed
  , lineBased
  -- * shared
  , eol
  , newline
  , whitespace
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
  ) where


import Text.ParserCombinators.Parsec
  ( Parser
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

import Text.Nast.AnnotatedAST
  ( Expr (..)
  , Stmt (..)
  , Annotations
  , CodeAnnotation (..)
  )


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
                   xs <- char '"' >> codeAnnotations
                   return $ StringLiteral s xs

-- | Stan expression
expression :: Parser Expr
expression = precedence10

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
  where p l' = do xs <- char '?' >> codeAnnotations
                  m <- precedence9
                  ys <- char ':' >> codeAnnotations
                  r <- precedence10
                  return $ Conditional l' xs m ys r

{-|
Precedence level 9 expressions

* logical @||@ op, binary infix, left associative; "or" disjunction
-}
precedence9 :: Parser Expr
precedence9 = chainl1 precedence8 $ binOp "||" Or

{-|
Precedence level 8 expressions

* logical @&&@ op, binary infix, left associative; "and" conjunction
-}
precedence8 :: Parser Expr
precedence8 = chainl1 precedence7 $ binOp "&&" And

{-|
Precedence level 7 expressions

* logical @==@ op, binary infix, left associative; equal to
* logical @!=@ op, binary infix, left associative; not equal to
-}
precedence7 :: Parser Expr
precedence7 = chainl1 precedence6 $ binOp "!=" NotEqual <|> binOp "==" Equal

{-|
Precedence level 6 expressions

* @<@  op, binary infix, left associative; smaller than
* @<=@ op, binary infix, left associative; smaller or equal than
* @>@  op, binary infix, left associative; greater than
* @>=@ op, binary infix, left associative; greater or equal than
-}
precedence6 :: Parser Expr
precedence6 = chainl1 precedence5 op
  where op = try (binOp "<=" Leq)
             <|> binOp "<" Lt
             <|> try (binOp ">=" Geq)
             <|> binOp ">" Gt

{-|
Precedence level 5 expressions

* @+@ op, binary infix, left associative; addition
* @-@ op, binary infix, left associative; subtraction
-}
precedence5 :: Parser Expr
precedence5 = chainl1 precedence4 $ binOp "+" Add <|> binOp "-" Sub

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
  where op = binOp "*" Mul
             <|> binOp "/" Div
             <|> try (binOp ".*" EltMul)
             <|> binOp "./" EltDiv

{-|
Precedence level 3 expressions

* @\@ op, binary infix, left associative; left division
* @%\%@ op, binary infix, left associative; integer division
-}
precedence3 :: Parser Expr
-- precedence3 = chainl1 precedence2 $ binOp ["\\", "%\\%"]
precedence3 = chainl1 precedence2 $ binOp "\\" LDiv <|> binOp "%\\%" IntDiv

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
  where completeBin f e = do xs <- codeAnnotations
                             r <- precedence1
                             return $ f e xs r

{-|
Precedence level 0 expressions

* @'@ op, postfix; matrix transposition
* @()@ function application
* @[]@ array & matrix indexing
-}
precedence0 :: Parser Expr
precedence0 = do e <- primary  -- Parse primary only once for efficiency
                 (t e) <|> (completeCall e) <|> (completeIndex e) <|> (return e)
  where t e = do xs <- char '\'' >> codeAnnotations
                 return $ Transpose e xs

{-|
Complete function call. We assume that the call is already consumed and passed
as an argument.
-}
completeCall :: Expr -> Parser Expr
completeCall = callLike '(' ')' Call expression

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
                          cs <- char rd >> codeAnnotations
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
parentheses = do xs <- char '(' >> codeAnnotations
                 e <- expression
                 ys <- char ')' >> codeAnnotations
                 return $ Parens xs e ys

{-| Type synonym for constructors of `Expr a` that represent binary Stan
operations.
-}
type BinConstr = Expr               -- ^ left-hand side
               -> Annotations       -- ^ annotations ofter op symbol
               -> Expr              -- ^ right-hand side
               -> Expr

{-| Parse annotated binary operator on expressions -}
binOp :: String -> BinConstr -> Parser (Expr -> Expr -> Expr)
binOp s c = do xs <- string s >> codeAnnotations
               return $ (\l r -> c l xs r)

-- | Zero or more comments and newlines
codeAnnotations :: Parser Annotations
codeAnnotations = whitespace >> (newline <|> comment) `sepEndBy` whitespace

-- | One or more comments and newlines
codeAnnotations1 :: Parser Annotations
codeAnnotations1 = try annotations <|> justWhitespaces
  where annotations = whitespace >> (newline <|> comment) `sepEndBy1` whitespace
        justWhitespaces = whitespace1 >> return []

-- | Stan comment
comment :: Parser CodeAnnotation
comment =   try lineBased
        <|> try bracketed
        <?> "comment"

-- | @//* ... *//@ style comment
bracketed :: Parser CodeAnnotation
bracketed = do c <- string "/*" >> manyTill anyChar (try $ string "*/")
               return $ Bracketed c

-- | @// ...@ style comment
lineBased :: Parser CodeAnnotation
lineBased = do c <- string "//" >> manyTill (noneOf "\n") eol
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
          <|> try printStmt
          <|> try reject
          <|> block
          <|> try ifElse
          <|> try for
          <|> try while
          <|> try targetPlusAssign
          <|> try assignment
          <|> try tilde
          <?> "statement"

-- | Parse keyword statement such as @break@ or @continue@
keyword :: String                                -- ^ keyword name
        -> (Annotations -> Annotations -> Stmt)  -- ^ constructor for Stmt
        -> Parser Stmt
keyword k constr = do xs <- string k >> codeAnnotations
                      ys <- char ';' >> codeAnnotations
                      return $ constr xs ys

-- | Parse block statement
block :: Parser Stmt
block = do xs <- char '{' >> codeAnnotations
           stmts <- many statement
           ys <- char '}' >> codeAnnotations
           return $ Block xs stmts ys

-- | Parse if then (else) statements
ifElse :: Parser Stmt
ifElse = do xs <- string "if" >> codeAnnotations
            cond <- parentheses
            stmt <- statement
            elseAnn <- optionMaybe $ try $ string "else" >> codeAnnotations
            case elseAnn of
              Nothing -> return $ If xs cond stmt
              Just ys -> do stmt' <- statement
                            return $ IfElse xs cond stmt ys stmt'

-- | Parse assignment statement ops such as @=@, @<-@, @+=@, @*=@
assignment :: Parser Stmt
assignment = do l <- lhs
                (xs, op) <- assignOp
                r <- expression
                ys <- char ';' >> codeAnnotations
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
  where p t g = try $ (,) <$> ((string t) >> codeAnnotations) <*> return g
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
for = do xs <- string "for" >> codeAnnotations
         ys <- char '(' >> codeAnnotations
         i <- identifier
         zs <- string "in" >> codeAnnotations
         l <- expression
         opt_r <- optionMaybe $ try $
                    char ':' >> (,) <$> codeAnnotations <*> expression
         vs <- char ')' >> codeAnnotations
         b <- statement
         case opt_r of
           (Just (ws, r)) -> return $ ForRange xs ys i zs l ws r vs b
           _ -> return $ For xs ys i zs l vs b

{-| Parse while-loop statement -}
while :: Parser Stmt
while = do xs <- string "while" >> codeAnnotations
           ys <- char '(' >> codeAnnotations
           cond <- expression
           zs <- char ')' >> codeAnnotations
           b <- statement
           return $ While xs ys cond zs b

{-| Tilde statement -}
tilde :: Parser Stmt
tilde = do l <- expression
           xs <- char '~' >> codeAnnotations
           i <- identifier
           r <- completeCall i
           ys <- char ';' >> codeAnnotations
           return $ Tilde l xs r ys

{-| Return statement -}
returnStmt :: Parser Stmt
returnStmt = do xs <- string "return" >> codeAnnotations
                e <- optionMaybe $ try expression
                ys <- char ';' >> codeAnnotations
                return $ Return xs e ys

{-| Print statement -}
printStmt :: Parser Stmt
printStmt = do xs <- string "print" >> codeAnnotations <* char '('
               ps <- printables
               ys <- char ')' >> codeAnnotations
               zs <- char ';' >> codeAnnotations
               return $ Print xs ps ys zs

{-| Reject statement -}
reject :: Parser Stmt
reject = do xs <- string "reject" >> codeAnnotations <* char '('
            ps <- printables
            ys <- char ')' >> codeAnnotations
            zs <- char ';' >> codeAnnotations
            return $ Reject xs ps ys zs

{-| Target plus-assign statement, e.g., @target += expr;@. -}
targetPlusAssign :: Parser Stmt
targetPlusAssign = do xs <- string "target" >> codeAnnotations
                      ys <- string "+=" >> codeAnnotations
                      e <- expression
                      zs <- char ';' >> codeAnnotations
                      return $ TargetPlusAssign xs ys e zs
