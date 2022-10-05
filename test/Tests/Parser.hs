module Tests.Parser (tests) where

import Text.Nast.Annotation (Annotation (..))
import Text.Nast.Expr (Expr (..))
import Text.Nast.Parser
  ( annotations
  , expression
  , identifier
  , literal
  , whitespace
  )

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.Either (isLeft)


tests :: [TestTree]
tests =
  [ testGroup "Precedence 5"
      [ testCase "3 + 4" $ parse expression "" "3 + 4" @?=
        (Right $ Add (NumLiteral "3" Nothing Nothing) [] (NumLiteral "4" Nothing Nothing))
      , testCase "0+1" $ parse expression "" "0+1" @?=
        (Right $ Add (NumLiteral "0" Nothing Nothing) [] (NumLiteral "1" Nothing Nothing))
      , testCase "annotated addition" $ parse expression "" "3 /*a*/ + /*b*/ /*c*/\n4 /*d*/" @?=
        (Right $ Add (Annotate (NumLiteral "3" Nothing Nothing) (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate (NumLiteral "4" Nothing Nothing) (Bracketed "d")))
      , testCase "1 +2 +  3" $ parse expression "" "1 +2 +  3" @?=
        (Right $ Add (Add (NumLiteral "1" Nothing Nothing) []
                          (NumLiteral "2" Nothing Nothing))
                     [] (NumLiteral "3" Nothing Nothing))
      , testCase "3 - 4" $ parse expression "" "3 - 4" @?=
        (Right $ Sub (NumLiteral "3" Nothing Nothing) [] (NumLiteral "4" Nothing Nothing))
      , testCase "0-1" $ parse expression "" "0-1" @?=
        (Right $ Sub (NumLiteral "0" Nothing Nothing) [] (NumLiteral "1" Nothing Nothing))
      , testCase "annotated subtraction" $ parse expression "" "3 /*a*/ - /*b*/ /*c*/\n4 /*d*/" @?=
        (Right $ Sub (Annotate (NumLiteral "3" Nothing Nothing) (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate (NumLiteral "4" Nothing Nothing) (Bracketed "d")))
      , testCase "1 -2 +  3" $ parse expression "" "1 -2 +  3" @?=
        (Right $ Add (Sub (NumLiteral "1" Nothing Nothing) []
                          (NumLiteral "2" Nothing Nothing))
                     [] (NumLiteral "3" Nothing Nothing))
      ]
  , testGroup "Precedence 4"
      [ testCase "3 * x" $ parse expression "" "3 * x" @?=
        (Right $ Mul (NumLiteral "3" Nothing Nothing) [] (Identifier "x"))
      , testCase "0*1" $ parse expression "" "0*1" @?=
        (Right $ Mul (NumLiteral "0" Nothing Nothing) []
                     (NumLiteral "1" Nothing Nothing))
      , testCase "annotated multiplication" $
        parse expression "" "3 /*a*/ * /*b*/ /*c*/\n4 /*d*/" @?=
        (Right $ Mul (Annotate (NumLiteral "3" Nothing Nothing) (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate (NumLiteral "4" Nothing Nothing)
                               (Bracketed "d")))
      , testCase "a * b + c" $ parse expression "" "a * b + c" @?=
        (Right $ Add (Mul (Identifier "a") [] (Identifier "b")) []
                     (Identifier "c"))
      , testCase "a + b / c" $ parse expression "" "a + b / c" @?=
        (Right $ Add (Identifier "a") []
                     (Div (Identifier "b") [] (Identifier "c")))
      , testCase "p .* q" $ parse expression "" "p .* q" @?=
        (Right $ EltMul (Identifier "p") [] (Identifier "q"))
      , testCase "p ./ q" $ parse expression "" "p ./ q" @?=
        (Right $ EltDiv (Identifier "p") [] (Identifier "q"))
      ]
  , testGroup "literal"
      [ testCase "23" $ parse literal "" "23" @?=
        (Right $ NumLiteral "23" Nothing Nothing)
      , testCase "-7" $ parse literal "" "-7" @?=
        (Right $ NumLiteral "-7" Nothing Nothing)
      , testCase "0" $ parse literal "" "0" @?=
        (Right $ NumLiteral "0" Nothing Nothing)
      , testCase "12.23" $ parse literal "" "12.23" @?=
        (Right $ NumLiteral "12" (Just "23") Nothing)
      , testCase "03.14" $ parse literal "" "03.14" @?=
        (Right $ NumLiteral "03" (Just "14") Nothing)
      , testCase "-13.1e07" $ parse literal "" "-13.1e07" @?=
        (Right $ NumLiteral "-13" (Just "1") (Just "07"))
      , testCase "3e6" $ parse literal "" "3e6" @?=
        (Right $ NumLiteral "3" Nothing (Just "6"))
      , testCase "-98e-012" $ parse literal "" "-98e-012" @?=
        (Right $ NumLiteral "-98" Nothing (Just "-012"))
      , testCase "003.001e006" $ parse literal "" "003.001e006" @?=
        (Right $ NumLiteral "003" (Just "001") (Just "006"))
      , testCase "'' (empty string)" $ parse literal "" "\"\"" @?=
        (Right $ StringLiteral "")
      , testCase "'abc'" $ parse literal "" "\"abc\"" @?=
        (Right $ StringLiteral "abc")
      , testCase "'a\\na' fails" $
        assertBool "" (isLeft $ parse literal "" "\"a\na\"")
      ]
  , testGroup "parentheses"
    [ testCase "(3)" $ parse expression "" "(3)" @?=
        (Right $ Parens [] $ NumLiteral "3" Nothing Nothing)
    , testCase "(//ab\\n 1 ) /*xy*/" $
        parse expression "" "(//ab\n 1 ) /*xy*/" @?=
        (Right $ Annotate (Parens [LineBased "ab"]
                                  (NumLiteral "1" Nothing Nothing))
                          (Bracketed "xy"))
    , testCase "(\\n3)" $ parse expression "" "(\n3)" @?=
        (Right $ Parens [Newline] (NumLiteral "3" Nothing Nothing))
    , testCase "(/*a*/ /*b*/\\n3)" $ parse expression "" "(/*a*/ /*b*/\n3)" @?=
        (Right $ Parens [Bracketed "a", Bracketed "b", Newline]
                        (NumLiteral "3" Nothing Nothing))
    ]
  , testGroup "identifier"
    [ testCase "a" $ parse expression "" "a" @?=
        (Right $ Identifier "a")
    , testCase "xyz" $ parse expression "" "xyz" @?=
        (Right $ Identifier "xyz")
    , testCase "xyZz12" $ parse expression "" "xyZz12" @?=
        (Right $ Identifier "xyZz12")
    , testCase "fail 1" $ assertBool "" (isLeft $ parse identifier "" "1")
    , testCase "fail 12ab" $ assertBool "" (isLeft $ parse identifier "" "12ab")
    , testCase "fail _xy" $ assertBool "" (isLeft $ parse identifier "" "_xy")
    , testCase "fail ab__" $ assertBool "" (isLeft $ parse identifier "" "ab__")
    ]
  , testGroup "annotations"
    [ testCase "//" $ parse annotations "" "//" @?= (Right [LineBased ""])
    , testCase "// abc" $ parse annotations "" "// abc" @?=
        (Right [LineBased " abc"])
    , testCase "/**/" $ parse annotations "" "/**/" @?= (Right [Bracketed ""])
    , testCase "/* abc */" $ parse annotations "" "/* abc */" @?=
        (Right [Bracketed " abc "])
    , testCase "/* ab*c*/" $ parse annotations "" "/* ab*c*/" @?=
        (Right [Bracketed " ab*c"])
    , testCase "/* a\\nb*c*/\\n//xy\\n//pq" $ parse annotations ""
        "/* a\nb*c*/\n//xy\n//pq" @?=
        (Right [Bracketed " a\nb*c", Newline, LineBased "xy", LineBased "pq"])
    , testCase "\\n" $ parse annotations "" "\n" @?= Right [Newline]
    ]
  , testGroup "annotated"
    [ testCase "literal 1 comment" $ parse literal "" "1//abc" @?=
      (Right $ Annotate (NumLiteral "1" Nothing Nothing) (LineBased "abc"))
    , testCase "literal 2 comments" $ parse literal "" "1 /* ab */ // xyz" @?=
      (Right $ Annotate
        (Annotate (NumLiteral "1" Nothing Nothing) (Bracketed " ab "))
        (LineBased " xyz"))
    ]
  , testGroup "whitespace"
    [ testCase "''" $ parse whitespace "" "" @?= Right ""
    , testCase "'  '" $ parse whitespace "" "  " @?= Right "  "
    , testCase "'\\t  \\t'" $ parse whitespace "" "\t  \t" @?= Right "\t  \t"
    , testCase "no newline" $ parse whitespace "" " \n" @?= Right " "
    ]
  ]
