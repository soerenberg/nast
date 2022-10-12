module Tests.Parser (tests) where

import Text.Nast.Annotation (ASTAnnotation (..), CodeAnnotation (..))
import Text.Nast.AST (Expr (..), Stmt (..))
import Text.Nast.Parser
  ( expression
  , identifier
  , literal
  , codeAnnotations
  , codeAnnotations1
  , range
  , whitespace
  , statement
  , lhs
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
      , testCase "x^p^q" $ parse expression "" "x^p^q" @?=
        (Right $ Pow id_x (Pow id_p id_q noBA) noBA)
      , testCase "x.^p.^q" $ parse expression "" "x.^p.^q" @?=
        (Right $ EltPow id_x (EltPow id_p id_q noBA) noBA)
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
  , testGroup "lhs"
      [ testCase "xyz" $ parse lhs "" "xyz" @?= (Right $ Identifier "xyz" noPA)
      , testCase "x[3]" $ parse lhs "" "x[3]" @?=
          (Right $ Index id_x [lit_3] (CallAnn [[]] []))
      , testCase "x[2,1]" $ parse lhs "" "x[3,1]" @?=
          (Right $ Index id_x [lit_3, lit_1] (CallAnn [[], []] []))
      , testCase "'-b' fails" $ assertBool "" (isLeft $ parse lhs "" "-b")
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
  , testGroup "Precedence 0"
    [ testCase "abc()" $ parse expression "" "abc()" @?=
        (Right $ Call (Identifier "abc" noPA) [] (CallAnn [[]] []))
    , testCase "f(/*xy*/)" $ parse expression "" "f(/*xy*/)" @?=
        (Right $ Call (Identifier "f" noPA) [] (CallAnn [[Bracketed "xy"]] []))
    , testCase "f(a, b , c)" $ parse expression "" "f(a, b , c)" @?=
        (Right $ Call (Identifier "f" noPA)
                      [ Identifier "a" noPA
                      , Identifier "b" noPA
                      , Identifier "c" noPA]
                      (CallAnn [[], [], []] []))
    , testCase "f /*ab*/ ( /*xy*/) /*pq*/" $
        parse expression "" "f /*ab*/ ( /*xy*/) /*pq*/" @?=
        (Right $ Call (Identifier "f" (PrimaryAnn [Bracketed "ab"]))
                      [] (CallAnn [[Bracketed "xy"]] [Bracketed "pq"]))
    , testCase "f(/*A*/a //x\\n,b , /*D*/ c)" $
        parse expression "" "f(/*A*/a //x\n,b , /*D*/ c)" @?=
        (Right $ Call (Identifier "f" noPA)
                      [ Identifier "a" (PrimaryAnn [LineBased "x"])
                      , Identifier "b" noPA
                      , Identifier "c" noPA]
                      (CallAnn [[Bracketed "A"], [], [Bracketed "D"]] []))
    , testCase "M'" $ parse expression "" "M'" @?=
        (Right $ Transpose (Identifier "M" noPA) (UnaryAnn []))
    , testCase "M/*a*/'/*b*/" $ parse expression "" "M/*a*/'/*b*/" @?=
        (Right $ Transpose (Identifier "M" (PrimaryAnn [Bracketed "a"]))
                           (UnaryAnn [Bracketed "b"]))
    , testCase "x[]" $ parse expression "" "x[]" @?=
        (Right $ Index id_x [] (CallAnn [[]] []))
    , testCase "x[/*A*/]" $ parse expression "" "x[/*A*/]" @?=
        (Right $ Index id_x [] (CallAnn [[Bracketed "A"]] []))
    , testCase "x[3]" $ parse expression "" "x[3]" @?=
        (Right $ Index id_x [lit_3] (CallAnn [[]] []))
    , testCase "x[2,1]" $ parse expression "" "x[3,1]" @?=
        (Right $ Index id_x [lit_3, lit_1] (CallAnn [[], []] []))
    , testCase "x [ 2 , 1 ]" $ parse expression "" "x [ 3 , 1 ]" @?=
        (Right $ Index id_x [lit_3, lit_1] (CallAnn [[], []] []))
    , testCase "x/*A*/[/*B*/a/*C*/,/*D*/b/*E*/]/*F*/" $
        parse expression "" "x/*A*/[/*B*/a/*C*/,/*D*/b/*E*/]/*F*/" @?=
        (Right $ Index (Identifier "x" (PrimaryAnn [Bracketed "A"]))
                       [ Identifier "a" (PrimaryAnn [Bracketed "C"])
                       , Identifier "b" (PrimaryAnn [Bracketed "E"])]
                       (CallAnn [[Bracketed "B"], [Bracketed "D"]]
                                [Bracketed "F"]))
    , testCase "x[a/*A*/:/*B*/b/*C*/]" $
        parse expression "" "x[a/*A*/:/*B*/b/*C*/]" @?=
        (Right $ Index id_x
            [ Range (Just $ Identifier "a" (PrimaryAnn [Bracketed "A"]))
                    (Just $ Identifier "b" (PrimaryAnn [Bracketed "C"]))
                    (BinaryAnn [Bracketed "B"])
            ] (CallAnn [[]] []))
    , testCase "x[a:b,p:q]" $
        parse expression "" "x[a:b,p:q]" @?=
        (Right $ Index id_x
            [ Range (Just $ id_a) (Just $ id_b) (BinaryAnn [])
            , Range (Just $ id_p) (Just $ id_q) (BinaryAnn [])
            ] (CallAnn [[], []] []))
    , testCase "x[a:b][p:q]" $
        parse expression "" "x[a:b][p:q]" @?=
        (Right $ Index
          (Index id_x [Range (Just $ id_a) (Just $ id_b) (BinaryAnn [])]
            (CallAnn [[]] []))
          [Range (Just $ id_p) (Just $ id_q) (BinaryAnn [])]
          (CallAnn [[]] []))
    , testCase "annotated x[a:b][p:q]" $
        parse expression ""
        "x/*A*/[/*B*/a/*C*/:/*D*/b/*E*/]/*F*/[/*G*/p/*H*/:/*I*/q/*J*/]/*K*/" @?=
        (Right $ Index
          (Index (Identifier "x" $ PrimaryAnn [Bracketed "A"])
                 [Range (Just $ Identifier "a" $ PrimaryAnn [Bracketed "C"])
                        (Just $ Identifier "b" $ PrimaryAnn [Bracketed "E"])
                        (BinaryAnn [Bracketed "D"])]
            (CallAnn [[Bracketed "B"]] [Bracketed "F"]))
          [Range (Just $ Identifier "p" $ PrimaryAnn [Bracketed "H"])
                 (Just $ Identifier "q" $ PrimaryAnn [Bracketed "J"])
                 (BinaryAnn [Bracketed "I"])]
          (CallAnn [[Bracketed "G"]] [Bracketed "K"]))
    ]
  , testGroup "range"
    [ testCase "range a" $ parse range "" "a" @?= (Right $ id_a)
    , testCase "range 1" $ parse range "" "1" @?= (Right $ lit_1)
    , testCase "range a:b" $ parse range "" "a:b" @?=
        (Right $ Range (Just id_a) (Just id_b) noBA)
    , testCase "range a:b" $ parse range "" "a:b" @?=
        (Right $ Range (Just id_a) (Just id_b) noBA)
    , testCase "range :b" $ parse range "" ":b" @?=
        (Right $ Range Nothing (Just id_b) noBA)
    , testCase "range a:" $ parse range "" "a:" @?=
        (Right $ Range (Just id_a) Nothing noBA)
    , testCase "range ':'" $ parse range "" ":" @?=
        (Right $ Range Nothing Nothing noBA)
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
    , testCase "' '" $ parse codeAnnotations "" " " @?= Right []
    , testCase "''" $ parse codeAnnotations "" " " @?= Right []
    ]
  , testGroup "codeAnnotations1"
    [ testCase "//" $ parse codeAnnotations1 "" "//" @?= (Right [LineBased ""])
    , testCase "// abc" $ parse codeAnnotations1 "" "// abc" @?=
        (Right [LineBased " abc"])
    , testCase "/**/" $ parse codeAnnotations1 "" "/**/" @?=
        (Right [Bracketed ""])
    , testCase "/* abc */" $ parse codeAnnotations1 "" "/* abc */" @?=
        (Right [Bracketed " abc "])
    , testCase "/* ab*c*/" $ parse codeAnnotations1 "" "/* ab*c*/" @?=
        (Right [Bracketed " ab*c"])
    , testCase "/* a\\nb*c*/\\n//xy\\n//pq" $ parse codeAnnotations1 ""
        "/* a\nb*c*/\n//xy\n//pq" @?=
        (Right [Bracketed " a\nb*c", Newline, LineBased "xy", LineBased "pq"])
    , testCase "\\n" $ parse codeAnnotations1 "" "\n" @?= Right [Newline]
    , testCase "' '" $ parse codeAnnotations1 "" " " @?= Right []
    , testCase "'' fails" $ assertBool ""
        (isLeft $ parse codeAnnotations1 "" "")
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
  , testGroup "break"
    [ testCase "break;" $ parse statement "" "break;" @?=
      (Right $ Break $ KeywordAnn [] [])
    , testCase "break/*A/*; /*B*/" $ parse statement "" "break/*A*/;/*B*/" @?=
      (Right $ Break $ KeywordAnn [Bracketed "A"] [Bracketed "B"])
    ]
  , testGroup "continue"
    [ testCase "continue;" $ parse statement "" "continue;" @?=
      (Right $ Continue $ KeywordAnn [] [])
    , testCase "continue/*A/*; /*B*/" $
      parse statement "" "continue/*A*/;/*B*/" @?=
      (Right $ Continue $ KeywordAnn [Bracketed "A"] [Bracketed "B"])
    ]
  , testGroup "return"
    [ testCase "return;" $ parse statement "" "return;" @?=
      (Right $ Return $ KeywordAnn [] [])
    , testCase "return/*A/*; /*B*/" $
      parse statement "" "return/*A*/;/*B*/" @?=
      (Right $ Return $ KeywordAnn [Bracketed "A"] [Bracketed "B"])
    ]
  , testGroup "block"
    [ testCase "{}" $ parse statement "" "{}" @?=
      (Right $ Block [] (BlockAnn [] []))
    , testCase "{return; continue; }" $
      parse statement "" "{return; continue; }" @?=
      (Right $ Block [Return (KeywordAnn [] []), Continue (KeywordAnn [] [])]
               (BlockAnn [] []))
    , testCase "{/*A*/return; } /*B*/" $
      parse statement "" "{/*A*/return; } /*B*/" @?=
      (Right $ Block [Return (KeywordAnn [] [])]
               (BlockAnn [Bracketed "A"] [Bracketed "B"]))
    ]
  , testGroup "if/else"
    [ testCase "if (a) return;" $ parse statement "" "if (a) return;"
      @?= (Right $ If (Parens id_a noPaA) (Return $ KeywordAnn [] [])
                      (IfAnn []))
    , testCase "if (a) return; else {}" $
      parse statement "" "if (a) return; else {}"
      @?= (Right $ IfElse (Parens id_a noPaA) (Return $ KeywordAnn [] [])
                          (Block [] (BlockAnn [] []))
                          (IfElseAnn [] []))
    , testCase "if/*A*/(a) return; else/*B*/{}" $
      parse statement "" "if/*A*/(a) return; else/*B*/{}"
      @?= (Right $ IfElse (Parens id_a noPaA) (Return $ KeywordAnn [] [])
                          (Block [] (BlockAnn [] []))
                          (IfElseAnn [Bracketed "A"] [Bracketed "B"]))
    ]
  , testGroup "assignments"
    [ testCase "a=b;" $ parse statement "" "a=b;" @?=
      (Right $ Assign id_a id_b $ AssignAnn [] [])
    , testCase "a = b ;" $ parse statement "" "a = b ;" @?=
      (Right $ Assign id_a id_b $ AssignAnn [] [])
    , testCase "annotated assign" $
      parse statement "" "a /*A*/ =\nb/*B*/ ; /*C*/" @?=
      (Right $ Assign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                      (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                      (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a<-b;" $ parse statement "" "a<-b;" @?=
      (Right $ ArrowAssign id_a id_b $ AssignAnn [] [])
    , testCase "a <- b ;" $ parse statement "" "a <- b ;" @?=
      (Right $ ArrowAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated arrow assign" $
      parse statement "" "a /*A*/ <-\nb/*B*/ ; /*C*/" @?=
      (Right $ ArrowAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                           (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                           (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a+=b;" $ parse statement "" "a+=b;" @?=
      (Right $ PlusAssign id_a id_b $ AssignAnn [] [])
    , testCase "a += b ;" $ parse statement "" "a += b ;" @?=
      (Right $ PlusAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated plus assign" $
      parse statement "" "a /*A*/ +=\nb/*B*/ ; /*C*/" @?=
      (Right $ PlusAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                          (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                          (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a-=b;" $ parse statement "" "a-=b;" @?=
      (Right $ MinusAssign id_a id_b $ AssignAnn [] [])
    , testCase "a -= b ;" $ parse statement "" "a -= b ;" @?=
      (Right $ MinusAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated minus assign" $
      parse statement "" "a /*A*/ -=\nb/*B*/ ; /*C*/" @?=
      (Right $ MinusAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                           (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                           (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a*=b;" $ parse statement "" "a*=b;" @?=
      (Right $ TimesAssign id_a id_b $ AssignAnn [] [])
    , testCase "a *= b ;" $ parse statement "" "a *= b ;" @?=
      (Right $ TimesAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated times assign" $
      parse statement "" "a /*A*/ *=\nb/*B*/ ; /*C*/" @?=
      (Right $ TimesAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                           (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                           (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a/=b;" $ parse statement "" "a/=b;" @?=
      (Right $ DivideAssign id_a id_b $ AssignAnn [] [])
    , testCase "a /= b ;" $ parse statement "" "a /= b ;" @?=
      (Right $ DivideAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated div assign" $
      parse statement "" "a /*A*/ /=\nb/*B*/ ; /*C*/" @?=
      (Right $ DivideAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                            (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                            (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a.*=b;" $ parse statement "" "a.*=b;" @?=
      (Right $ EltTimesAssign id_a id_b $ AssignAnn [] [])
    , testCase "a .*= b ;" $ parse statement "" "a .*= b ;" @?=
      (Right $ EltTimesAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated elttimes assign" $
      parse statement "" "a /*A*/ .*=\nb/*B*/ ; /*C*/" @?=
      (Right $ EltTimesAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                           (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                           (AssignAnn [Newline] [Bracketed "C"]))
    , testCase "a./=b;" $ parse statement "" "a./=b;" @?=
      (Right $ EltDivideAssign id_a id_b $ AssignAnn [] [])
    , testCase "a ./= b ;" $ parse statement "" "a ./= b ;" @?=
      (Right $ EltDivideAssign id_a id_b $ AssignAnn [] [])
    , testCase "annotated eltdiv assign" $
      parse statement "" "a /*A*/ ./=\nb/*B*/ ; /*C*/" @?=
      (Right $ EltDivideAssign (Identifier "a" $ PrimaryAnn [Bracketed "A"])
                               (Identifier "b" $ PrimaryAnn [Bracketed "B"])
                               (AssignAnn [Newline] [Bracketed "C"]))
    ]
  , testGroup "for loop"
    [ testCase "for (x in a) return;" $
      parse statement "" "for (x in a) return;" @?=
      (Right $ For id_x id_a (Return $ KeywordAnn [] [])
                   (ForAnn [] [] [] []))
    , testCase "annotated For" $
      parse statement "" "for/*A*/(/*B*/x in/*C*/ a)/*D*/ return;" @?=
      (Right $ For id_x id_a (Return $ KeywordAnn [] [])
                   (ForAnn [Bracketed "A"] [Bracketed "B"] [Bracketed "C"]
                           [Bracketed "D"]))
    , testCase "for (x in 1:3) return;" $
      parse statement "" "for (x in 1:3) return;" @?=
      (Right $ ForRange id_x lit_1 lit_3 (Return $ KeywordAnn [] [])
                        (ForRangeAnn [] [] [] [] []))
    , testCase "annotated ForRange" $
      parse statement "" "for/*A*/(/*B*/x in/*C*/ a:/*D*/b)/*E*/ return;" @?=
      (Right $ ForRange id_x id_a id_b (Return $ KeywordAnn [] [])
                   (ForRangeAnn [Bracketed "A"] [Bracketed "B"] [Bracketed "C"]
                                [Bracketed "D"] [Bracketed "E"]))
    ]
  , testGroup "while loop"
    [ testCase "while (a) return;" $ parse statement "" "while (a) return;" @?=
      (Right $ While (Parens id_a $ ParensAnn [] [])
                     (Return $ KeywordAnn [] []) $ WhileAnn [])
    , testCase "while/*A*/(a) return;" $
      parse statement "" "while/*A*/(a) return;"
      @?= (Right $ While (Parens id_a $ ParensAnn [] [])
                         (Return $ KeywordAnn [] [])
                         (WhileAnn [Bracketed "A"]))
    ]
  , testGroup "special cases"
    [ testCase "returnn = a;" $ parse statement "" "returnn = a;" @?=
      (Right $ Assign (Identifier "returnn" noPA) id_a $ AssignAnn [] [])
    , testCase "break_ = a;" $ parse statement "" "break_ = a;" @?=
      (Right $ Assign (Identifier "break_" noPA) id_a $ AssignAnn [] [])
    , testCase "continue_ = a;" $ parse statement "" "continue_ = a;" @?=
      (Right $ Assign (Identifier "continue_" noPA) id_a $ AssignAnn [] [])
    , testCase "if_ = a;" $ parse statement "" "if_ = a;" @?=
      (Right $ Assign (Identifier "if_" noPA) id_a $ AssignAnn [] [])
    , testCase "else_ = a;" $ parse statement "" "else_ = a;" @?=
      (Right $ Assign (Identifier "else_" noPA) id_a $ AssignAnn [] [])
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

noPaA :: ASTAnnotation   -- no parentheses annotation
noPaA = ParensAnn [] []

noUA :: ASTAnnotation   -- no unary annotation
noUA = UnaryAnn []
