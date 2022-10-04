module Text.Nast.Parser (
  -- * annotations
    annotations
  , annotate
  , bracketed
  , lineBased
  -- * literals
  , literal
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

import Text.Nast.Expr (Expr (..))
import Text.Nast.Annotation (Annotation (..))


-- | Zero or more comments and newlines
annotations :: Parser [Annotation]
annotations = whitespace >> (newline <|> comment) `sepEndBy` whitespace

-- | Parse annotations, create tree with expression as leaf
annotate :: Expr Annotation -> Parser (Expr Annotation)
annotate e = do xs <- annotations
                return $ foldl Annotate e xs

-- | Stan comment
comment :: Parser Annotation
comment =   try lineBased
        <|> try bracketed
        <?> "comment"

-- | @//* ... *//@ style comment
bracketed :: Parser Annotation
bracketed = do _ <- string "/*"
               c <- manyTill anyChar (try $ string "*/")
               return $ Bracketed c

-- | @// ...@ style comment
lineBased :: Parser Annotation
lineBased = do _ <- string "//"
               c <- manyTill (noneOf "\n") eol
               return $ LineBased c

-- | Stan literal
literal :: Parser (Expr Annotation)
literal = numLiteral <|> stringLiteral

numLiteral :: Parser (Expr Annotation)
numLiteral = do i <- signedInt
                d <- optionMaybe (char '.' >> many1 digit)
                e <- optionMaybe (char 'e' >> signedInt)
                annotate $ NumLiteral i d e

signedInt :: Parser String
signedInt = do plus <|> minus <|> nosign
    where nosign = many1 digit
          plus = (:) <$> char '+' <*> nosign
          minus = (:) <$> char '-' <*> nosign

stringLiteral :: Parser (Expr Annotation)
stringLiteral = do _ <- char '"'
                   s <- many $ noneOf "\n\""
                   _ <- char '"'
                   annotate $ StringLiteral s

expression :: Parser (Expr Annotation)
expression = precedence10

{-|
Precedence level 10 expressions

* Ternary @?~:@ op, ternary infix, right associative;

Here, right associative means that @a ? b : c ? d : e@ is equivalent to
@a ? b : (c ? d : e)@.
-}
precedence10 :: Parser (Expr Annotation)
precedence10 = precedence9

{-|
Precedence level 9 expressions

* logical @||@ op, binary infix, left associative; "or" disjunction
-}
precedence9 :: Parser (Expr Annotation)
precedence9 = precedence8

{-|
Precedence level 8 expressions

* logical @&&@ op, binary infix, left associative; "and" conjunction
-}
precedence8 :: Parser (Expr Annotation)
precedence8 = precedence7

{-|
Precedence level 7 expressions

* logical @==@ op, binary infix, left associative; equal to
* logical @!=@ op, binary infix, left associative; not equal to
-}
precedence7 :: Parser (Expr Annotation)
precedence7 = precedence6

{-|
Precedence level 6 expressions

* @<@  op, binary infix, left associative; smaller than
* @<=@ op, binary infix, left associative; smaller or equal than
* @>@  op, binary infix, left associative; greater than
* @>=@ op, binary infix, left associative; greater or equal than
-}
precedence6 :: Parser (Expr Annotation)
precedence6 = precedence5

{-|
Precedence level 5 expressions

* @+@ op, binary infix, left associative; addition
* @-@ op, binary infix, left associative; subtraction
-}
precedence5 :: Parser (Expr Annotation)
precedence5 = chainl1 precedence4 p
  where p :: Parser (Expr Annotation -> Expr Annotation -> Expr Annotation)
        p = do s <- oneOf "+-"
               let op = if s == '+' then Add else Sub
               xs <- annotations
               return $ (\l r -> op l xs r)

{-|
Precedence level 4 expressions

* @*@ op, binary infix, left associative; multiplication
* @.*@ op, binary infix, left associative; element-wise multiplication
* @/@ op, binary infix, left associative; division
* @./@ op, binary infix, left associative; element-wise division
* @%@ op, binary infix, left associative; modulo
-}
precedence4 :: Parser (Expr Annotation)
precedence4 = precedence3

{-|
Precedence level 3 expressions

* @\@ op, binary infix, left associative; left division
* @%\%@ op, binary infix, left associative; integer division
-}
precedence3 :: Parser (Expr Annotation)
precedence3 = precedence2

{-|
Precedence level 2 expressions

* @!@ op, unary prefix; logical negation
* @+@ op, unary prefix; promotion (no-op)
* @-@ op, unary prefix; negation
-}
precedence2 :: Parser (Expr Annotation)
precedence2 = precedence1

{-|
Precedence level 1 expressions

* @^@ op, binary infix, right associative; exponentiation
* @.^@ op, binary infix, right associative; element-wise exponentiation
-}
precedence1 :: Parser (Expr Annotation)
precedence1 = precedence0

{-|
Precedence level 0 expressions

* @'@ op, postfix; matrix transposition
* @()@ function application
* @[]@ array & matrix indexing
-}
precedence0 :: Parser (Expr Annotation)
precedence0 = primary

primary :: Parser (Expr Annotation)
primary = literal <|> parentheses <|> identifier

{-|
Identifier

The following constraints must hold:
* Only @a-z@, @A-Z@, @_@, @0-9@ allowed.
* Must start with letter.
* Must not end with two underscores: @__@.
-}
identifier :: Parser (Expr Annotation)
identifier = do x <- letter
                xs <- many $ letter <|> digit <|> char '_'
                let p = take 2 . reverse $ xs
                if p == "__"
                  then fail "identifier must not end with '__'"
                  else annotate $ Identifier (x:xs)

parentheses :: Parser (Expr Annotation)
parentheses = do _ <- char '('
                 xs <- annotations
                 e <- expression
                 _ <- char ')'
                 annotate $ Parens xs e

eol :: Parser ()
eol = (lookAhead eof) <|> (void $ char '\n') <?> "end of line"

newline :: Parser Annotation
newline = char '\n' >> return Newline <?> "newline"

-- | Zero or more space or tab characters
whitespace :: Parser String
whitespace = many $ oneOf " \t"
