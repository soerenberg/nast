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
  , binOp
  , binConstr
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
import qualified Data.Map as Map

import Text.Nast.AST (Expr (..), Stmt (..))
import Text.Nast.Annotation (ASTAnnotation (..), CodeAnnotation (..))


-- | Stan numeric literal
numLiteral :: Parser (Expr ASTAnnotation)
numLiteral = do i <- signedInt
                d <- optionMaybe (char '.' >> many1 digit)
                e <- optionMaybe (char 'e' >> signedInt)
                xs <- codeAnnotations
                return $ NumLiteral i d e (PrimaryAnn xs)

signedInt :: Parser String
signedInt = do plus <|> minus <|> nosign
    where nosign = many1 digit
          plus = (:) <$> char '+' <*> nosign
          minus = (:) <$> char '-' <*> nosign

stringLiteral :: Parser (Expr ASTAnnotation)
stringLiteral = do _ <- char '"'
                   s <- many $ noneOf "\n\""
                   _ <- char '"'
                   xs <- codeAnnotations
                   return $ StringLiteral s (PrimaryAnn xs)

expression :: Parser (Expr ASTAnnotation)
expression = precedence10

{-| List of printables. Used in @print@ and @reject@ statements. -}
printables :: Parser (Expr ASTAnnotation)
printables = do xs <- p `sepBy1` char ','
                let (as, es) = unzip xs
                return $ Printables es $ PrintablesAnn as
  where p = (,) <$> codeAnnotations <*> (stringLiteral <|> expression)

{-|
Precedence level 10 expressions

* Ternary @?:@ op, ternary infix, right associative;

Here, right associative means that @a ? b : c ? d : e@ is equivalent to
@a ? b : (c ? d : e)@.
-}
precedence10 :: Parser (Expr ASTAnnotation)
-- Here we try to ensure that the precedence9 expression is only parsed once.
-- For a parser of the form @try conditional <|> precedence9@ this might happen
-- twice if there is in fact no ternary @?:@ operation. However, this is
-- unnecessarily expensive if the precedence9 expression is complex.
precedence10 = do l <- precedence9
                  p l <|> return l
  where p l' = do _ <- char '?'
                  xs <- codeAnnotations
                  m <- precedence9
                  _ <- char ':'
                  ys <- codeAnnotations
                  r <- precedence10
                  return $ Conditional l' m r (CondAnn xs ys)

{-|
Precedence level 9 expressions

* logical @||@ op, binary infix, left associative; "or" disjunction
-}
precedence9 :: Parser (Expr ASTAnnotation)
precedence9 = chainl1 precedence8 $ p
  where p = do _ <- string "||"
               xs <- codeAnnotations
               return $ (\l r -> Or l r (BinaryAnn xs))

{-|
Precedence level 8 expressions

* logical @&&@ op, binary infix, left associative; "and" conjunction
-}
precedence8 :: Parser (Expr ASTAnnotation)
precedence8 = chainl1 precedence7 $ p
  where p = do _ <- string "&&"
               xs <- codeAnnotations
               return $ (\l r -> And l r (BinaryAnn xs))

{-|
Precedence level 7 expressions

* logical @==@ op, binary infix, left associative; equal to
* logical @!=@ op, binary infix, left associative; not equal to
-}
precedence7 :: Parser (Expr ASTAnnotation)
precedence7 = chainl1 precedence6 $ binOp ["!=", "=="]

{-|
Precedence level 6 expressions

* @<@  op, binary infix, left associative; smaller than
* @<=@ op, binary infix, left associative; smaller or equal than
* @>@  op, binary infix, left associative; greater than
* @>=@ op, binary infix, left associative; greater or equal than
-}
precedence6 :: Parser (Expr ASTAnnotation)
precedence6 = chainl1 precedence5 $ binOp ["<=", "<", ">=", ">"]

{-|
Precedence level 5 expressions

* @+@ op, binary infix, left associative; addition
* @-@ op, binary infix, left associative; subtraction
-}
precedence5 :: Parser (Expr ASTAnnotation)
precedence5 = chainl1 precedence4 $ binOp ["+", "-"]

{-|
Precedence level 4 expressions

* @*@ op, binary infix, left associative; multiplication
* @.*@ op, binary infix, left associative; element-wise multiplication
* @/@ op, binary infix, left associative; division
* @./@ op, binary infix, left associative; element-wise division
* @%@ op, binary infix, left associative; modulo
-}
precedence4 :: Parser (Expr ASTAnnotation)
precedence4 = chainl1 precedence3 $ binOp ["*", "/", ".*", "./"]

{-|
Precedence level 3 expressions

* @\@ op, binary infix, left associative; left division
* @%\%@ op, binary infix, left associative; integer division
-}
precedence3 :: Parser (Expr ASTAnnotation)
precedence3 = chainl1 precedence2 $ binOp ["\\", "%\\%"]

{-|
Precedence level 2 expressions

* @!@ op, unary prefix; logical negation
* @+@ op, unary prefix; promotion (no-op)
* @-@ op, unary prefix; negation
-}
precedence2 :: Parser (Expr ASTAnnotation)
precedence2 = lneg <|> plus <|> minus <|> precedence1
  where lneg  = char '!' >> flip LogicalNeg <$> unaryAnn <*> precedence2
        plus  = char '+' >> flip UnaryPlus <$> unaryAnn <*> precedence2
        minus = char '-' >> flip UnaryMinus <$> unaryAnn <*> precedence2
        unaryAnn = do xs <- codeAnnotations
                      return $ UnaryAnn xs

{-|
Precedence level 1 expressions

* @^@ op, binary infix, right associative; exponentiation
* @.^@ op, binary infix, right associative; element-wise exponentiation
-}
precedence1 :: Parser (Expr ASTAnnotation)
precedence1 = do e <- precedence0
                 (try $ char '^' >> completeBin Pow e) <|>
                   (try $ string ".^" >> completeBin EltPow e) <|>
                   return e
  where completeBin f e = do xs <- codeAnnotations
                             r <- precedence1
                             return $ f e r (BinaryAnn xs)

{-|
Precedence level 0 expressions

* @'@ op, postfix; matrix transposition
* @()@ function application
* @[]@ array & matrix indexing
-}
precedence0 :: Parser (Expr ASTAnnotation)
precedence0 = do e <- primary  -- Parse primary only once for efficiency
                 (t e) <|> (completeCall e) <|> (completeIndex e) <|> (return e)
  where t e = do _ <- char '\''
                 xs <- codeAnnotations
                 return $ Transpose e (UnaryAnn xs)

{-|
Complete function call. We assume that the call is already consumed and passed
as an argument.
-}
completeCall :: Expr ASTAnnotation -> Parser (Expr ASTAnnotation)
completeCall = callLike '(' ')' Call expression

{-|
Complete array/vector/matrix expression. We assume that the object to be indexed
is already consumed and passed as an argument.
-}
completeIndex :: Expr ASTAnnotation -> Parser (Expr ASTAnnotation)
completeIndex e = do e' <- callLike '[' ']' Index range e
                     (completeIndex e' <|> return e')

{-| Type synonym for type of 'Range' and 'Index' constructors -}
type CallLikeConstr = Expr ASTAnnotation
                    -> [Expr ASTAnnotation]
                    -> ASTAnnotation
                    -> Expr ASTAnnotation

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
         -> (Parser (Expr ASTAnnotation))
         -> (Expr ASTAnnotation)           -- ^ Identifier (callee).
         -> Parser (Expr ASTAnnotation)
callLike ld rd f p e = do _ <- char ld
                          (argAnns, args) <- (try withArgs) <|> withoutArgs
                          _ <- char rd
                          cs <- codeAnnotations
                          return $ f e args (CallAnn argAnns cs)
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
range :: Parser (Expr ASTAnnotation)
range = do l <- optionMaybe $ try expression
           colon <- optionMaybe $ char ':'
           case (l, colon) of
             (Just e,  Nothing) -> return e
             (Nothing, Nothing) -> fail "Cannot parse range"
             _ -> do cs <- codeAnnotations
                     r <- optionMaybe $ try expression
                     return $ Range l r (BinaryAnn cs)

{-| Left-hand side expression

Expressions that are allowed on the left-hand side of assignments are
identifiers with zero or more array/matrix indices, such as @idfr@, @idfr[3]@,
@idfr[3][5]@ and so on
-}
lhs :: Parser (Expr ASTAnnotation)
lhs = do i <- identifier
         (completeIndex i) <|> (return i)

primary :: Parser (Expr ASTAnnotation)
primary = numLiteral <|> parentheses <|> identifier

{-|
Identifier

The following constraints must hold:
* Only @a-z@, @A-Z@, @_@, @0-9@ allowed.
* Must start with letter.
* Must not end with two underscores: @__@.
-}
identifier :: Parser (Expr ASTAnnotation)
identifier = do x <- letter
                xs <- many $ letter <|> digit <|> char '_'
                let p = take 2 . reverse $ xs
                ys <- codeAnnotations
                if p == "__"
                  then fail "identifier must not end with '__'"
                  else return $ Identifier (x:xs) (PrimaryAnn ys)

parentheses :: Parser (Expr ASTAnnotation)
parentheses = do _ <- char '('
                 xs <- codeAnnotations
                 e <- expression
                 _ <- char ')'
                 ys <- codeAnnotations
                 return $ Parens e (ParensAnn xs ys)

{-| Type synonym for binary operation on expressions (`Expr`). -}
type BinOp = Expr ASTAnnotation  -- ^ left-hand side
           -> Expr ASTAnnotation -- ^ right-hand side
           -> Expr ASTAnnotation

{-| Type synonym for constructors of `Expr a` that represent binary Stan
operations.
-}
type BinConstr = Expr ASTAnnotation  -- ^ left-hand side
               -> Expr ASTAnnotation -- ^ right-hand side
               -> ASTAnnotation      -- ^ annotations ofter op symbol
               -> Expr ASTAnnotation

{-|
Parse binary operation on `Expr a` operation, with annotations for the
underlying infix symbol. Thus, this is a helper function to parse binary
stan ops

For a given list of expected symbols will return the first successfully parsed
candidate. Thus, @binConstr ["<", "<="]@ and @binConstr ["<=", "<"]@ are
generally not equivalent (the latter is probably the desired call).
-}
binOp :: [String] -> Parser BinOp
binOp xs = do op <- binConstr xs
              case op of
                -- Note: than op is Nothing should never happen in practice
                Nothing -> fail $ "Cannot identify binary op in " ++ (show xs)
                (Just op') -> do as <- codeAnnotations
                                 return (\l r -> op' l r (BinaryAnn as))

{-|
Parse constructor of binary `Expr` operation, e.g. `Add`, `Mul`.

For a given list of expected symbols will return the first successfully parsed
candidate. Thus, @binConstr ["<", "<="]@ and @binConstr ["<=", "<"]@ are
generally not equivalent (the latter is probably the desired call).
-}
binConstr :: [String]  -- ^ List of expected symbols, e.g. @["+", "-"]@.
          -> Parser (Maybe BinConstr)
binConstr xs = (flip Map.lookup toBinOp) <$>
                 (foldl1 (<|>) $ map (try . string) xs)
  where toBinOp :: Map.Map String BinConstr
        toBinOp = Map.fromList
          [ ("==",   Equal)
          , ("!=",   NotEqual)
          , (">",    Gt)
          , (">=",   Geq)
          , ("<",    Lt)
          , ("<=",   Leq)
          , ("+",    Add)
          , ("-",    Sub)
          , ("*",    Mul)
          , ("/",    Div)
          , (".*",   EltMul)
          , ("./",   EltDiv)
          , ("\\",   LDiv)
          , ("%\\%", IntDiv)
          , ("^",    Pow)
          , (".^",   EltPow)
          ]

-- | Zero or more comments and newlines
codeAnnotations :: Parser [CodeAnnotation]
codeAnnotations = whitespace >> (newline <|> comment) `sepEndBy` whitespace

-- | One or more comments and newlines
codeAnnotations1 :: Parser [CodeAnnotation]
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
bracketed = do _ <- string "/*"
               c <- manyTill anyChar (try $ string "*/")
               return $ Bracketed c

-- | @// ...@ style comment
lineBased :: Parser CodeAnnotation
lineBased = do _ <- string "//"
               c <- manyTill (noneOf "\n") eol
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
statement :: Parser (Stmt ASTAnnotation)
statement =   (try $ keyword "break" Break)
          <|> (try $ keyword "continue" Continue)
          <|> (try $ keyword "return" Return)
          <|> block
          <|> try ifElse
          <|> try for
          <|> try while
          <|> try assignment
          <|> try tilde
          <?> "statement"

-- | Parse keyword statement such as @break@ or @continue@
keyword :: String                                 -- ^ keyword name
        -> (ASTAnnotation -> Stmt ASTAnnotation)  -- ^ constructor for Stmt
        -> Parser (Stmt ASTAnnotation)
keyword k constr = do _ <- string k
                      xs <- codeAnnotations
                      _ <- char ';'
                      ys <- codeAnnotations
                      return $ constr $ KeywordAnn xs ys

-- | Parse block statement
block :: Parser (Stmt ASTAnnotation)
block = do _ <- char '{'
           xs <- codeAnnotations
           stmts <- many statement
           _ <- char '}'
           ys <- codeAnnotations
           return $ Block stmts $ BlockAnn xs ys

-- | Parse if then (else) statements
ifElse :: Parser (Stmt ASTAnnotation)
ifElse = do _ <- string "if"
            xs <- codeAnnotations
            cond <- parentheses
            stmt <- statement
            elseAnn <- optionMaybe $ try $ string "else" >> codeAnnotations
            case elseAnn of
              Nothing -> return $ If cond stmt $ IfAnn xs
              Just ys -> do stmt' <- statement
                            return $ IfElse cond stmt stmt' $ IfElseAnn xs ys

-- | Parse assignment statement ops such as @=@, @<-@, @+=@, @*=@
assignment :: Parser (Stmt ASTAnnotation)
assignment = do l <- lhs
                (xs, op) <- assignOp
                r <- expression
                ys <- char ';' >> codeAnnotations
                return $ op l r $ AssignAnn xs ys

{-| Type synonym for construrtors assignment (Assign, PlusAssign, ...) nodes -}
type AssignConstr = Expr ASTAnnotation   -- ^ left-hand side
                  -> Expr ASTAnnotation  -- ^ right-hand side
                  -> ASTAnnotation       -- ^ annotations after op token
                  -> Stmt ASTAnnotation

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
for :: Parser (Stmt ASTAnnotation)
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
           (Just (ws, r)) -> return $ ForRange i l r b $
                                      ForRangeAnn xs ys zs ws vs
           _ -> return $ For i l b (ForAnn xs ys zs vs)

{-| Parse while-loop statement -}
while :: Parser (Stmt ASTAnnotation)
while = do xs <- string "while" >> codeAnnotations
           cond <- parentheses
           b <- statement
           return $ While cond b $ WhileAnn xs

{-| Tilde statement -}
tilde :: Parser (Stmt ASTAnnotation)
tilde = do l <- expression
           xs <- char '~' >> codeAnnotations
           i <- identifier
           r <- completeCall i
           ys <- char ';' >> codeAnnotations
           return $ Tilde l r $ TildeAnn xs ys
