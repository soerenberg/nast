module Tests.Parser (tests) where

import Text.Nast.Annotation (ASTAnnotation (..), CodeAnnotation (..))
import Text.Nast.Expr (Expr (..))
import Text.Nast.Parser
  ( expression
  , identifier
  , literal
  , codeAnnotations
  , whitespace
  )

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.Either (isLeft)



tests :: [TestTree]
tests =
  [ testGroup "Precedence 10"
      [ testCase "a ? b : c" $ parse expression "" "a ? b : c" @?=
        (Right $ Conditional id_a id_b id_c (CondAnn [] []))
      , testCase "a?b:c" $ parse expression "" "a?b:c" @?=
        (Right $ Conditional id_a id_b  id_c (CondAnn [] []))
      , testCase "associativity" $ parse expression "" "a?b:c?p:q" @?=
        (Right $ Conditional
                   id_a id_b (Conditional id_c id_p id_q (CondAnn [] []))
                   (CondAnn [] []))
      ]
  , testGroup "Precedence 9"
      [ testCase "a || b||c" $ parse expression "" "a || b||c" @?=
        (Right $ Or (Or id_a id_b noBA) id_c noBA)
      , testCase "a|| b ||c" $ parse expression "" "a|| b ||c" @?=
        (Right $ Or (Or id_a id_b noBA) id_c noBA)
      , testCase "annotated ||" $ parse expression ""
                                  "x /*a*/ || /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Or (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                    (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                    (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      ]
  , testGroup "Precedence 8"
      [ testCase "a && b&&c" $ parse expression "" "a && b&&c" @?=
        (Right $ And (And id_a id_b (BinaryAnn [])) id_c (BinaryAnn []))
      , testCase "a&& b &&c" $ parse expression "" "a&& b &&c" @?=
        (Right $ And (And id_a id_b (BinaryAnn [])) id_c (BinaryAnn []))
      , testCase "annotated &&" $ parse expression ""
                                  "x /*a*/ && /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ And (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                     (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      ]
  , testGroup "Precedence 7"
      [ testCase "a == b==c" $ parse expression "" "a == b==c" @?=
        (Right $ Equal (Equal id_a id_b noBA) id_c noBA)
      , testCase "a != b!=c" $ parse expression "" "a != b!=c" @?=
        (Right $ NotEqual (NotEqual id_a id_b noBA) id_c noBA)
      , testCase "a != b==c" $ parse expression "" "a != b==c" @?=
        (Right $ Equal (NotEqual id_a id_b noBA) id_c noBA)
      , testCase "a== b !=c" $ parse expression "" "a== b !=c" @?=
        (Right $ NotEqual (Equal id_a id_b noBA) id_c noBA)
      , testCase "annotated ==" $ parse expression ""
                                  "x /*a*/ == /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Equal (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                       (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                       (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "annotated !=" $ parse expression ""
                                  "x /*a*/ != /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ NotEqual (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                          (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                          (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      ]
  , testGroup "Precedence 6"
      [ testCase "a > b > c" $ parse expression "" "a > b > c" @?=
        (Right $ Gt (Gt id_a id_b noBA) id_c noBA)
      , testCase "a >= b >= c" $ parse expression "" "a >= b >= c" @?=
        (Right $ Geq (Geq id_a id_b noBA) id_c noBA)
      , testCase "a < b < c" $ parse expression "" "a < b < c" @?=
        (Right $ Lt (Lt id_a id_b noBA) id_c noBA)
      , testCase "a <= b <= c" $ parse expression "" "a <= b <= c" @?=
        (Right $ Leq (Leq id_a id_b noBA) id_c noBA)
      , testCase "annotated Gt" $ parse expression ""
                                  "x /*a*/ > /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Gt (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                    (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                    (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "annotated Geq" $ parse expression ""
                                  "x /*a*/ >= /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Geq (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                     (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "annotated Lt" $ parse expression ""
                                  "x /*a*/ < /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Lt (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                    (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                    (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "annotated Leq" $ parse expression ""
                                  "x /*a*/ <= /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Leq (Identifier "x" (PrimaryAnn [Bracketed "a"]))
                     (Identifier "y" (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      ]
  , testGroup "Precedence 5"
      [ testCase "p + q" $ parse expression "" "p + q" @?=
        (Right $ Add id_p id_q noBA)
      , testCase "0+1" $ parse expression "" "0+1" @?=
        (Right $ Add lit_0 lit_1 noBA)
      , testCase "annotated addition" $
        parse expression "" "3 /*a*/ + /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Add (NumLiteral "3" Nothing Nothing
                       (PrimaryAnn [Bracketed "a"]))
                     (NumLiteral "1" Nothing Nothing
                       (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "1 +2 +  3" $ parse expression "" "1 +2 +  3" @?=
        (Right $ Add (Add lit_1 lit_2 noBA) lit_3 noBA)
      , testCase "3 - 4" $ parse expression "" "3 - 4" @?=
        (Right $ Sub lit_3 (NumLiteral "4" Nothing Nothing noPA) noBA)
      , testCase "0-1" $ parse expression "" "0-1" @?=
        (Right $ Sub lit_0 lit_1 noBA)
      , testCase "annotated subtraction" $
        parse expression "" "3 /*a*/ - /*b*/ /*c*/\n2 /*d*/" @?=
        (Right $ Sub (NumLiteral "3" Nothing Nothing
                       (PrimaryAnn [Bracketed "a"]))
                     (NumLiteral "2" Nothing Nothing
                       (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "1 -2 +  3" $ parse expression "" "1 -2 +  3" @?=
        (Right $ Add (Sub lit_1 lit_2 noBA) lit_3 noBA)
      ]
  , testGroup "Precedence 4"
      [ testCase "3 * x" $ parse expression "" "3 * x" @?=
        (Right $ Mul lit_3 id_x noBA)
      , testCase "0*1" $ parse expression "" "0*1" @?=
        (Right $ Mul lit_0 lit_1 noBA)
      , testCase "annotated multiplication" $
        parse expression "" "3 /*a*/ * /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Mul (NumLiteral "3" Nothing Nothing
                       (PrimaryAnn [Bracketed "a"]))
                     (NumLiteral "1" Nothing Nothing
                       (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "a * b + c" $ parse expression "" "a * b + c" @?=
        (Right $ Add (Mul id_a id_b noBA) id_c noBA)
      , testCase "a + b / c" $ parse expression "" "a + b / c" @?=
        (Right $ Add id_a (Div id_b id_c noBA) noBA)
      , testCase "p .* q" $ parse expression "" "p .* q" @?=
        (Right $ EltMul id_p id_q noBA)
      , testCase "p ./ q" $ parse expression "" "p ./ q" @?=
        (Right $ EltDiv id_p id_q noBA)
      ]
  , testGroup "Precedence 3"
      [ testCase "3 \\ x" $ parse expression "" "3 \\ x" @?=
        (Right $ LDiv lit_3 id_x noBA)
      , testCase "0%\\%1" $ parse expression "" "0%\\%1" @?=
        (Right $ IntDiv lit_0 lit_1 noBA)
      , testCase "annotated int div" $
        parse expression "" "3 /*a*/ %\\% /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ IntDiv (NumLiteral "3" Nothing Nothing
                                    (PrimaryAnn [Bracketed "a"]))
                        (NumLiteral "1" Nothing Nothing
                                    (PrimaryAnn [Bracketed "d"]))
                        (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "a \\ b + c" $ parse expression "" "a \\ b + c" @?=
        (Right $ Add (LDiv id_a id_b noBA) id_c noBA)
      , testCase "a + b %\\% c" $ parse expression "" "a + b %\\% c" @?=
        (Right $ Add id_a (IntDiv id_b id_c noBA) noBA)
      ]
  , testGroup "Precedence 3"
      [ testCase "3^x" $ parse expression "" "3^x" @?=
        (Right $ Pow lit_3 id_x noBA)
      , testCase "p .^ 1" $ parse expression "" "p .^ 1" @?=
        (Right $ EltPow id_p lit_1 noBA)
      , testCase "p.^ 1" $ parse expression "" "p.^ 1" @?=
        (Right $ EltPow id_p lit_1 noBA)
      , testCase "a .^2" $ parse expression "" "a .^2" @?=
        (Right $ EltPow id_a lit_2 noBA)
      , testCase "a.^2" $ parse expression "" "a.^2" @?=
        (Right $ EltPow id_a lit_2 noBA)
      , testCase "annotated pow" $
        parse expression "" "3 /*a*/ ^ /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Pow (NumLiteral "3" Nothing Nothing
                                     (PrimaryAnn [Bracketed "a"]))
                     (NumLiteral "1" Nothing Nothing
                                     (PrimaryAnn [Bracketed "d"]))
                     (BinaryAnn [Bracketed "b", Bracketed "c", Newline]))
      , testCase "a ^ b + c" $ parse expression "" "a ^ b + c" @?=
        (Right $ Add (Pow id_a id_b noBA) id_c noBA)
      , testCase "a + b .^ c" $ parse expression "" "a + b .^ c" @?=
        (Right $ Add id_a (EltPow id_b id_c noBA) noBA)
      ]
  , testGroup "Precedence 2"
      [ testCase "!a" $ parse expression "" "!a" @?=
        (Right $ LogicalNeg id_a noUA)
      , testCase "+a" $ parse expression "" "+a" @?=
        (Right $ UnaryPlus id_a noUA)
      , testCase "-a" $ parse expression "" "-a" @?=
        (Right $ UnaryMinus id_a noUA)
      , testCase "-!+a" $ parse expression "" "-!+a" @?=
        (Right $ UnaryMinus (LogicalNeg (UnaryPlus id_a noUA) noUA) noUA)
      , testCase "+ /*xy*/ p //uv" $ parse expression "" "+ /*xy*/ p //uv" @?=
        (Right $ UnaryPlus (Identifier "p" (PrimaryAnn [LineBased "uv"]))
                           (UnaryAnn [Bracketed "xy"]))
      , testCase "- /*xy*/ p //uv" $ parse expression "" "- /*xy*/ p //uv" @?=
        (Right $ UnaryMinus (Identifier "p" (PrimaryAnn [LineBased "uv"]))
                            (UnaryAnn [Bracketed "xy"]))
      , testCase "! /*xy*/ p //uv" $ parse expression "" "! /*xy*/ p //uv" @?=
        (Right $ LogicalNeg (Identifier "p" (PrimaryAnn [LineBased "uv"]))
                            (UnaryAnn [Bracketed "xy"]))
      ]
  , testGroup "literal"
      [ testCase "23" $ parse literal "" "23" @?=
        (Right $ NumLiteral "23" Nothing Nothing noPA)
      , testCase "-7" $ parse literal "" "-7" @?=
        (Right $ NumLiteral "-7" Nothing Nothing noPA)
      , testCase "0" $ parse literal "" "0" @?=
        (Right $ NumLiteral "0" Nothing Nothing noPA)
      , testCase "12.23" $ parse literal "" "12.23" @?=
        (Right $ NumLiteral "12" (Just "23") Nothing noPA)
      , testCase "03.14" $ parse literal "" "03.14" @?=
        (Right $ NumLiteral "03" (Just "14") Nothing noPA)
      , testCase "-13.1e07" $ parse literal "" "-13.1e07" @?=
        (Right $ NumLiteral "-13" (Just "1") (Just "07") noPA)
      , testCase "3e6" $ parse literal "" "3e6" @?=
        (Right $ NumLiteral "3" Nothing (Just "6") noPA)
      , testCase "-98e-012" $ parse literal "" "-98e-012" @?=
        (Right $ NumLiteral "-98" Nothing (Just "-012") noPA)
      , testCase "003.001e006" $ parse literal "" "003.001e006" @?=
        (Right $ NumLiteral "003" (Just "001") (Just "006") noPA)
      , testCase "'' (empty string)" $ parse literal "" "\"\"" @?=
        (Right $ StringLiteral "" noPA)
      , testCase "'abc'" $ parse literal "" "\"abc\"" @?=
        (Right $ StringLiteral "abc" noPA)
      , testCase "'a\\na' fails" $
        assertBool "" (isLeft $ parse literal "" "\"a\na\"")
      ]
  , testGroup "parentheses"
    [ testCase "(3)" $ parse expression "" "(3)" @?=
        (Right $ Parens (NumLiteral "3" Nothing Nothing noPA) (ParensAnn [] []))
    , testCase "(//ab\\n 1 ) /*xy*/" $
        parse expression "" "(//ab\n 1 ) /*xy*/" @?=
        (Right $ Parens lit_1 (ParensAnn [LineBased "ab"] [Bracketed "xy"]))
    , testCase "(\\n3)" $ parse expression "" "(\n3)" @?=
        (Right $ Parens lit_3 (ParensAnn [Newline] []))
    , testCase "(/*a*/ /*b*/\\n3)" $ parse expression "" "(/*a*/ /*b*/\n3)" @?=
        (Right $ Parens lit_3
                        (ParensAnn [Bracketed "a", Bracketed "b", Newline] []))
    ]
  , testGroup "identifier"
    [ testCase "a" $ parse expression "" "a" @?=
        (Right $ Identifier "a" noPA)
    , testCase "xyz" $ parse expression "" "xyz" @?=
        (Right $ Identifier "xyz" noPA)
    , testCase "xyZz12" $ parse expression "" "xyZz12" @?=
        (Right $ Identifier "xyZz12" noPA)
    , testCase "fail 1" $ assertBool "" (isLeft $ parse identifier "" "1")
    , testCase "fail 12ab" $ assertBool "" (isLeft $ parse identifier "" "12ab")
    , testCase "fail _xy" $ assertBool "" (isLeft $ parse identifier "" "_xy")
    , testCase "fail ab__" $ assertBool "" (isLeft $ parse identifier "" "ab__")
    ]
  , testGroup "codeAnnotations"
    [ testCase "//" $ parse codeAnnotations "" "//" @?= (Right [LineBased ""])
    , testCase "// abc" $ parse codeAnnotations "" "// abc" @?=
        (Right [LineBased " abc"])
    , testCase "/**/" $ parse codeAnnotations "" "/**/" @?=
        (Right [Bracketed ""])
    , testCase "/* abc */" $ parse codeAnnotations "" "/* abc */" @?=
        (Right [Bracketed " abc "])
    , testCase "/* ab*c*/" $ parse codeAnnotations "" "/* ab*c*/" @?=
        (Right [Bracketed " ab*c"])
    , testCase "/* a\\nb*c*/\\n//xy\\n//pq" $ parse codeAnnotations ""
        "/* a\nb*c*/\n//xy\n//pq" @?=
        (Right [Bracketed " a\nb*c", Newline, LineBased "xy", LineBased "pq"])
    , testCase "\\n" $ parse codeAnnotations "" "\n" @?= Right [Newline]
    ]
  , testGroup "annotated"
    [ testCase "literal 1 comment" $ parse literal "" "1//abc" @?=
      (Right $ (NumLiteral "1" Nothing Nothing (PrimaryAnn [LineBased "abc"])))
    , testCase "literal 2 comments" $ parse literal "" "1 /* ab */ // xyz" @?=
      (Right $ (NumLiteral "1" Nothing Nothing
                           (PrimaryAnn [Bracketed " ab ", LineBased " xyz"])))
    ]
  , testGroup "whitespace"
    [ testCase "''" $ parse whitespace "" "" @?= Right ""
    , testCase "'  '" $ parse whitespace "" "  " @?= Right "  "
    , testCase "'\\t  \\t'" $ parse whitespace "" "\t  \t" @?= Right "\t  \t"
    , testCase "no newline" $ parse whitespace "" " \n" @?= Right " "
    ]
  ]


{- definitions of expressions, for readability -}
id_a :: Expr ASTAnnotation
id_a = Identifier "a" noPA

id_b :: Expr ASTAnnotation
id_b = Identifier "b" noPA

id_c :: Expr ASTAnnotation
id_c = Identifier "c" noPA

id_x :: Expr ASTAnnotation
id_x = Identifier "x" noPA

id_p :: Expr ASTAnnotation
id_p = Identifier "p" noPA

id_q :: Expr ASTAnnotation
id_q = Identifier "q" noPA

lit_0 :: Expr ASTAnnotation
lit_0 = NumLiteral "0" Nothing Nothing noPA

lit_1 :: Expr ASTAnnotation
lit_1 = NumLiteral "1" Nothing Nothing noPA

lit_2 :: Expr ASTAnnotation
lit_2 = NumLiteral "2" Nothing Nothing noPA

lit_3 :: Expr ASTAnnotation
lit_3 = NumLiteral "3" Nothing Nothing noPA

noBA :: ASTAnnotation   -- no binary annotation
noBA = BinaryAnn []

noPA :: ASTAnnotation   -- no primary annotation
noPA = PrimaryAnn []

noUA :: ASTAnnotation   -- no unary annotation
noUA = UnaryAnn []
