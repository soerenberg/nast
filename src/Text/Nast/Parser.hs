module Text.Nast.Parser (
  -- * literals
    literal
  , numLiteral
  , signedInt
  , stringLiteral
  -- * expressions
  , expression
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
  , primary
  , identifier
  , parentheses
  , binOp
  , binConstr
  -- * code annotations
  , codeAnnotations
  , comment
  , bracketed
  , lineBased
  -- * shared
  , eol
  , newline
  , whitespace
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
  , sepEndBy
  , string
  , try
  )
import Control.Monad (void)
import qualified Data.Map as Map

import Text.Nast.Expr (Expr (..))
import Text.Nast.Annotation (ASTAnnotation (..), CodeAnnotation (..))


-- | Stan literal
literal :: Parser (Expr ASTAnnotation)
literal = numLiteral <|> stringLiteral

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
precedence1 = chainl1 precedence0 $ binOp [".^", "^"]

{-|
Precedence level 0 expressions

* @'@ op, postfix; matrix transposition
* @()@ function application
* @[]@ array & matrix indexing
-}
precedence0 :: Parser (Expr ASTAnnotation)
precedence0 = primary

primary :: Parser (Expr ASTAnnotation)
primary = literal <|> parentheses <|> identifier

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
