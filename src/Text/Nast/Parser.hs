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
  , char
  , digit
  , eof
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

eol :: Parser ()
eol = (lookAhead eof) <|> (void $ char '\n') <?> "end of line"

newline :: Parser Annotation
newline = char '\n' >> return Newline <?> "newline"

-- | Zero or more space or tab characters
whitespace :: Parser String
whitespace = many $ oneOf " \t"
