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
  [ testGroup "Precedence 6"
      [ testCase "a > b > c" $ parse expression "" "a > b > c" @?=
        (Right $ Gt (Gt id_a [] id_b) [] id_c)
      , testCase "a >= b >= c" $ parse expression "" "a >= b >= c" @?=
        (Right $ Geq (Geq id_a [] id_b) [] id_c)
      , testCase "a < b < c" $ parse expression "" "a < b < c" @?=
        (Right $ Lt (Lt id_a [] id_b) [] id_c)
      , testCase "a <= b <= c" $ parse expression "" "a <= b <= c" @?=
        (Right $ Leq (Leq id_a [] id_b) [] id_c)
      , testCase "annotated Gt" $ parse expression ""
                                  "x /*a*/ > /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Gt (Annotate id_x (Bracketed "a"))
                    [Bracketed "b", Bracketed "c", Newline]
                    (Annotate id_y (Bracketed "d")))
      , testCase "annotated Geq" $ parse expression ""
                                  "x /*a*/ >= /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Geq (Annotate id_x (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate id_y (Bracketed "d")))
      , testCase "annotated Lt" $ parse expression ""
                                  "x /*a*/ < /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Lt (Annotate id_x (Bracketed "a"))
                    [Bracketed "b", Bracketed "c", Newline]
                    (Annotate id_y (Bracketed "d")))
      , testCase "annotated Leq" $ parse expression ""
                                  "x /*a*/ <= /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Leq (Annotate id_x (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate id_y (Bracketed "d")))

      ]
  , testGroup "Precedence 5"
      [ testCase "p + q" $ parse expression "" "p + q" @?=
        (Right $ Add id_p [] id_q)
      , testCase "0+1" $ parse expression "" "0+1" @?=
        (Right $ Add lit_0 [] lit_1)
      , testCase "annotated addition" $
        parse expression "" "3 /*a*/ + /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Add (Annotate lit_3 (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate lit_1 (Bracketed "d")))
      , testCase "1 +2 +  3" $ parse expression "" "1 +2 +  3" @?=
        (Right $ Add (Add lit_1 [] lit_2) [] lit_3)
      , testCase "3 - 4" $ parse expression "" "3 - 4" @?=
        (Right $ Sub lit_3 [] (NumLiteral "4" Nothing Nothing))
      , testCase "0-1" $ parse expression "" "0-1" @?=
        (Right $ Sub lit_0 [] lit_1)
      , testCase "annotated subtraction" $
        parse expression "" "3 /*a*/ - /*b*/ /*c*/\n2 /*d*/" @?=
        (Right $ Sub (Annotate lit_3 (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate lit_2 (Bracketed "d")))
      , testCase "1 -2 +  3" $ parse expression "" "1 -2 +  3" @?=
        (Right $ Add (Sub lit_1 [] lit_2) [] lit_3)
      ]
  , testGroup "Precedence 4"
      [ testCase "3 * x" $ parse expression "" "3 * x" @?=
        (Right $ Mul lit_3 [] id_x)
      , testCase "0*1" $ parse expression "" "0*1" @?=
        (Right $ Mul lit_0 [] lit_1)
      , testCase "annotated multiplication" $
        parse expression "" "3 /*a*/ * /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Mul (Annotate lit_3 (Bracketed "a"))
                     [Bracketed "b", Bracketed "c", Newline]
                     (Annotate lit_1 (Bracketed "d")))
      , testCase "a * b + c" $ parse expression "" "a * b + c" @?=
        (Right $ Add (Mul id_a [] id_b) [] id_c)
      , testCase "a + b / c" $ parse expression "" "a + b / c" @?=
        (Right $ Add id_a [] (Div id_b [] id_c))
      , testCase "p .* q" $ parse expression "" "p .* q" @?=
        (Right $ EltMul id_p [] id_q)
      , testCase "p ./ q" $ parse expression "" "p ./ q" @?=
        (Right $ EltDiv id_p [] id_q)
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
        (Right $ Annotate (Parens [LineBased "ab"] lit_1) (Bracketed "xy"))
    , testCase "(\\n3)" $ parse expression "" "(\n3)" @?=
        (Right $ Parens [Newline] lit_3)
    , testCase "(/*a*/ /*b*/\\n3)" $ parse expression "" "(/*a*/ /*b*/\n3)" @?=
        (Right $ Parens [Bracketed "a", Bracketed "b", Newline] lit_3)
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
      (Right $ Annotate lit_1 (LineBased "abc"))
    , testCase "literal 2 comments" $ parse literal "" "1 /* ab */ // xyz" @?=
      (Right $ Annotate (Annotate lit_1 (Bracketed " ab ")) (LineBased " xyz"))
    ]
  , testGroup "whitespace"
    [ testCase "''" $ parse whitespace "" "" @?= Right ""
    , testCase "'  '" $ parse whitespace "" "  " @?= Right "  "
    , testCase "'\\t  \\t'" $ parse whitespace "" "\t  \t" @?= Right "\t  \t"
    , testCase "no newline" $ parse whitespace "" " \n" @?= Right " "
    ]
  ]


{- definitions of expressions, for readability -}
id_a :: Expr Annotation
id_a = Identifier "a"

id_b :: Expr Annotation
id_b = Identifier "b"

id_c :: Expr Annotation
id_c = Identifier "c"

id_x :: Expr Annotation
id_x = Identifier "x"

id_y :: Expr Annotation
id_y = Identifier "y"

id_p :: Expr Annotation
id_p = Identifier "p"

id_q :: Expr Annotation
id_q = Identifier "q"

lit_0 :: Expr Annotation
lit_0 = NumLiteral "0" Nothing Nothing

lit_1 :: Expr Annotation
lit_1 = NumLiteral "1" Nothing Nothing

lit_2 :: Expr Annotation
lit_2 = NumLiteral "2" Nothing Nothing

lit_3 :: Expr Annotation
lit_3 = NumLiteral "3" Nothing Nothing
