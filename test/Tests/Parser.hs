module Tests.Parser (tests) where

import Text.Nast.AnnotatedAST
  ( Expr (..)
  , Stmt (..)
  , CodeAnnotation (..)
  , VarDecl (..)
  , VarType (..)
  , VarConstraints (..)
  , VarConstraint (..)
  )

import Text.Nast.Parser
  ( expression
  , printables
  , identifier
  , numLiteral
  , stringLiteral
  , codeAnnotations
  , codeAnnotations1
  , range
  , whitespace
  , statement
  , lhs
  , varDeclaration
  , varType
  )

import Text.ParserCombinators.Parsec (Parser, parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.Either (isLeft)



expectParseFail :: Parser a  -- ^ parser to use
                -> String    -- ^ test case name
                -> String    -- ^ source to parse
                -> TestTree
expectParseFail p t s = testCase t $ assertBool "" $ isLeft $ parse p "" s

tests :: [TestTree]
tests =
  [ testGroup "Precedence 10"
      [ testCase "a ? b : c" $ parse expression "" "a ? b : c" @?=
        (Right $ Conditional id_a [] id_b [] id_c)
      , testCase "a?b:c" $ parse expression "" "a?b:c" @?=
        (Right $ Conditional id_a [] id_b [] id_c)
      , testCase "associativity" $ parse expression "" "a?b:c?p:q" @?=
        (Right $ Conditional id_a [] id_b [] (Conditional id_c [] id_p [] id_q))
      ]
  , testGroup "Precedence 9"
      [ testCase "a || b||c" $ parse expression "" "a || b||c" @?=
        (Right $ Or (Or id_a [] id_b) [] id_c)
      , testCase "a|| b ||c" $ parse expression "" "a|| b ||c" @?=
        (Right $ Or (Or id_a [] id_b) [] id_c)
      , testCase "annotated ||" $ parse expression ""
                                  "x /*a*/ || /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Or (Identifier "x" [Bracketed "a"])
                    [Bracketed "b", Bracketed "c", Newline]
                    (Identifier "y" [Bracketed "d"]))
      ]
  , testGroup "Precedence 8"
      [ testCase "a && b&&c" $ parse expression "" "a && b&&c" @?=
        (Right $ And (And id_a [] id_b) [] id_c)
      , testCase "a&& b &&c" $ parse expression "" "a&& b &&c" @?=
        (Right $ And (And id_a [] id_b) [] id_c)
      , testCase "annotated &&" $ parse expression ""
                                  "x /*a*/ && /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ And (Identifier "x" [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (Identifier "y" [Bracketed "d"]))
      ]
  , testGroup "Precedence 7"
      [ testCase "a == b==c" $ parse expression "" "a == b==c" @?=
        (Right $ Equal (Equal id_a [] id_b) [] id_c)
      , testCase "a != b!=c" $ parse expression "" "a != b!=c" @?=
        (Right $ NotEqual (NotEqual id_a [] id_b) [] id_c)
      , testCase "a != b==c" $ parse expression "" "a != b==c" @?=
        (Right $ Equal (NotEqual id_a [] id_b) [] id_c)
      , testCase "a== b !=c" $ parse expression "" "a== b !=c" @?=
        (Right $ NotEqual (Equal id_a [] id_b) [] id_c)
      , testCase "annotated ==" $ parse expression ""
                                  "x /*a*/ == /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Equal (Identifier "x" [Bracketed "a"])
                       [Bracketed "b", Bracketed "c", Newline]
                       (Identifier "y" [Bracketed "d"]))
      , testCase "annotated !=" $ parse expression ""
                                  "x /*a*/ != /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ NotEqual (Identifier "x" [Bracketed "a"])
                          [Bracketed "b", Bracketed "c", Newline]
                          (Identifier "y" [Bracketed "d"]))
      ]
  , testGroup "Precedence 6"
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
        (Right $ Gt (Identifier "x" [Bracketed "a"])
                    [Bracketed "b", Bracketed "c", Newline]
                    (Identifier "y" [Bracketed "d"]))
      , testCase "annotated Geq" $ parse expression ""
                                  "x /*a*/ >= /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Geq (Identifier "x" [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (Identifier "y" [Bracketed "d"]))
      , testCase "annotated Lt" $ parse expression ""
                                  "x /*a*/ < /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Lt (Identifier "x" [Bracketed "a"])
                    [Bracketed "b", Bracketed "c", Newline]
                    (Identifier "y" [Bracketed "d"]))
      , testCase "annotated Leq" $ parse expression ""
                                  "x /*a*/ <= /*b*/ /*c*/\ny /*d*/" @?=
        (Right $ Leq (Identifier "x" [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (Identifier "y" [Bracketed "d"]))
      ]
  , testGroup "Precedence 5"
      [ testCase "p + q" $ parse expression "" "p + q" @?=
        (Right $ Add id_p [] id_q)
      , testCase "0+1" $ parse expression "" "0+1" @?=
        (Right $ Add lit_0 [] lit_1)
      , testCase "annotated addition" $
        parse expression "" "3 /*a*/ + /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Add (NumLiteral "3" Nothing Nothing [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (NumLiteral "1" Nothing Nothing [Bracketed "d"]))
      , testCase "1 +2 +  3" $ parse expression "" "1 +2 +  3" @?=
        (Right $ Add (Add lit_1 [] lit_2) [] lit_3)
      , testCase "3 - 4" $ parse expression "" "3 - 4" @?=
        (Right $ Sub lit_3 [] (NumLiteral "4" Nothing Nothing []))
      , testCase "0-1" $ parse expression "" "0-1" @?=
        (Right $ Sub lit_0 [] lit_1)
      , testCase "annotated subtraction" $
        parse expression "" "3 /*a*/ - /*b*/ /*c*/\n2 /*d*/" @?=
        (Right $ Sub (NumLiteral "3" Nothing Nothing [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (NumLiteral "2" Nothing Nothing [Bracketed "d"]))
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
        (Right $ Mul (NumLiteral "3" Nothing Nothing [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (NumLiteral "1" Nothing Nothing [Bracketed "d"]))
      , testCase "a * b + c" $ parse expression "" "a * b + c" @?=
        (Right $ Add (Mul id_a [] id_b) [] id_c)
      , testCase "a + b / c" $ parse expression "" "a + b / c" @?=
        (Right $ Add id_a [] (Div id_b [] id_c))
      , testCase "p .* q" $ parse expression "" "p .* q" @?=
        (Right $ EltMul id_p [] id_q)
      , testCase "p ./ q" $ parse expression "" "p ./ q" @?=
        (Right $ EltDiv id_p [] id_q)
      ]
  , testGroup "Precedence 3"
      [ testCase "3 \\ x" $ parse expression "" "3 \\ x" @?=
        (Right $ LDiv lit_3 [] id_x)
      , testCase "0%\\%1" $ parse expression "" "0%\\%1" @?=
        (Right $ IntDiv lit_0 [] lit_1)
      , testCase "annotated int div" $
        parse expression "" "3 /*a*/ %\\% /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ IntDiv (NumLiteral "3" Nothing Nothing [Bracketed "a"])
                        [Bracketed "b", Bracketed "c", Newline]
                        (NumLiteral "1" Nothing Nothing [Bracketed "d"]))
      , testCase "a \\ b + c" $ parse expression "" "a \\ b + c" @?=
        (Right $ Add (LDiv id_a [] id_b) [] id_c)
      , testCase "a + b %\\% c" $ parse expression "" "a + b %\\% c" @?=
        (Right $ Add id_a [] (IntDiv id_b [] id_c))
      ]
  , testGroup "Precedence 3"
      [ testCase "3^x" $ parse expression "" "3^x" @?=
        (Right $ Pow lit_3 [] id_x)
      , testCase "x^p^q" $ parse expression "" "x^p^q" @?=
        (Right $ Pow id_x [] (Pow id_p [] id_q))
      , testCase "x.^p.^q" $ parse expression "" "x.^p.^q" @?=
        (Right $ EltPow id_x [] (EltPow id_p [] id_q))
      , testCase "p .^ 1" $ parse expression "" "p .^ 1" @?=
        (Right $ EltPow id_p [] lit_1)
      , testCase "p.^ 1" $ parse expression "" "p.^ 1" @?=
        (Right $ EltPow id_p [] lit_1)
      , testCase "a .^2" $ parse expression "" "a .^2" @?=
        (Right $ EltPow id_a [] lit_2)
      , testCase "a.^2" $ parse expression "" "a.^2" @?=
        (Right $ EltPow id_a [] lit_2)
      , testCase "annotated pow" $
        parse expression "" "3 /*a*/ ^ /*b*/ /*c*/\n1 /*d*/" @?=
        (Right $ Pow (NumLiteral "3" Nothing Nothing [Bracketed "a"])
                     [Bracketed "b", Bracketed "c", Newline]
                     (NumLiteral "1" Nothing Nothing [Bracketed "d"]))
      , testCase "a ^ b + c" $ parse expression "" "a ^ b + c" @?=
        (Right $ Add (Pow id_a [] id_b) [] id_c)
      , testCase "a + b .^ c" $ parse expression "" "a + b .^ c" @?=
        (Right $ Add id_a [] (EltPow id_b [] id_c))
      ]
  , testGroup "Precedence 2"
      [ testCase "!a" $ parse expression "" "!a" @?=
        (Right $ LogicalNeg [] id_a)
      , testCase "+a" $ parse expression "" "+a" @?=
        (Right $ UnaryPlus [] id_a)
      , testCase "-a" $ parse expression "" "-a" @?=
        (Right $ UnaryMinus [] id_a)
      , testCase "-!+a" $ parse expression "" "-!+a" @?=
        (Right $ UnaryMinus [] $ LogicalNeg [] $ UnaryPlus [] id_a)
      , testCase "+ /*xy*/ p //uv" $ parse expression "" "+ /*xy*/ p //uv" @?=
        (Right $ UnaryPlus [Bracketed "xy"] (Identifier "p" [LineBased "uv"]))
      , testCase "- /*xy*/ p //uv" $ parse expression "" "- /*xy*/ p //uv" @?=
        (Right $ UnaryMinus [Bracketed "xy"] (Identifier "p" [LineBased "uv"]))
      , testCase "! /*xy*/ p //uv" $ parse expression "" "! /*xy*/ p //uv" @?=
        (Right $ LogicalNeg [Bracketed "xy"] (Identifier "p" [LineBased "uv"]))
      ]
  , testGroup "lhs"
      [ testCase "xyz" $ parse lhs "" "xyz" @?= (Right $ Identifier "xyz" [])
      , testCase "x[3]" $ parse lhs "" "x[3]" @?=
          (Right $ Index id_x [lit_3] [[]] [])
      , testCase "x[2,1]" $ parse lhs "" "x[3,1]" @?=
          (Right $ Index id_x [lit_3, lit_1] [[], []] [])
      , expectParseFail lhs "'-b' fails" "-b"
      ]
  , testGroup "numeric literal"
      [ testCase "23" $ parse numLiteral "" "23" @?=
        (Right $ NumLiteral "23" Nothing Nothing [])
      , testCase "-7" $ parse numLiteral "" "-7" @?=
        (Right $ NumLiteral "-7" Nothing Nothing [])
      , testCase "0" $ parse numLiteral "" "0" @?=
        (Right $ NumLiteral "0" Nothing Nothing [])
      , testCase "12.23" $ parse numLiteral "" "12.23" @?=
        (Right $ NumLiteral "12" (Just "23") Nothing [])
      , testCase "03.14" $ parse numLiteral "" "03.14" @?=
        (Right $ NumLiteral "03" (Just "14") Nothing [])
      , testCase "-13.1e07" $ parse numLiteral "" "-13.1e07" @?=
        (Right $ NumLiteral "-13" (Just "1") (Just "07") [])
      , testCase "3e6" $ parse numLiteral "" "3e6" @?=
        (Right $ NumLiteral "3" Nothing (Just "6") [])
      , testCase "-98e-012" $ parse numLiteral "" "-98e-012" @?=
        (Right $ NumLiteral "-98" Nothing (Just "-012") [])
      , testCase "003.001e006" $ parse numLiteral "" "003.001e006" @?=
        (Right $ NumLiteral "003" (Just "001") (Just "006") [])
      ]
  , testGroup "string literals"
      [ testCase "'' (empty string)" $ parse stringLiteral "" "\"\"" @?=
        (Right $ StringLiteral "" [])
      , testCase "'abc'" $ parse stringLiteral "" "\"abc\"" @?=
        (Right $ StringLiteral "abc" [])
      , expectParseFail stringLiteral "'a\\na' fails" "\"a\na\""
      ]
  , testGroup "parentheses"
    [ testCase "(3)" $ parse expression "" "(3)" @?=
        (Right $ Parens [] (NumLiteral "3" Nothing Nothing []) [])
    , testCase "(//ab\\n 1 ) /*xy*/" $
        parse expression "" "(//ab\n 1 ) /*xy*/" @?=
        (Right $ Parens [LineBased "ab"] lit_1 [Bracketed "xy"])
    , testCase "(\\n3)" $ parse expression "" "(\n3)" @?=
        (Right $ Parens [Newline] lit_3 [])
    , testCase "(/*a*/ /*b*/\\n3)" $ parse expression "" "(/*a*/ /*b*/\n3)" @?=
        (Right $ Parens [Bracketed "a", Bracketed "b", Newline] lit_3 [])
    ]
  , testGroup "identifier"
    [ testCase "a" $ parse expression "" "a" @?=
        (Right $ Identifier "a" [])
    , testCase "xyz" $ parse expression "" "xyz" @?=
        (Right $ Identifier "xyz" [])
    , testCase "xyZz12" $ parse expression "" "xyZz12" @?=
        (Right $ Identifier "xyZz12" [])
    , expectParseFail identifier "fail i)" "1"
    , expectParseFail identifier "fail ii)" "12ab"
    , expectParseFail identifier "fail iii)" "_xy"
    , expectParseFail identifier "fail iv)" "ab__"
    ]
  , testGroup "Precedence 0"
    [ testCase "abc()" $ parse expression "" "abc()" @?=
        (Right $ Call (Identifier "abc" []) [] [[]] [])
    , testCase "f(/*xy*/)" $ parse expression "" "f(/*xy*/)" @?=
        (Right $ Call (Identifier "f" []) [] [[Bracketed "xy"]] [])
    , testCase "f(a, b , c)" $ parse expression "" "f(a, b , c)" @?=
        (Right $ Call (Identifier "f" [])
                      [ Identifier "a" []
                      , Identifier "b" []
                      , Identifier "c" []]
                      [[], [], []] [])
    , testCase "f /*ab*/ ( /*xy*/) /*pq*/" $
        parse expression "" "f /*ab*/ ( /*xy*/) /*pq*/" @?=
        (Right $ Call (Identifier "f" [Bracketed "ab"]) [] [[Bracketed "xy"]]
                      [Bracketed "pq"])
    , testCase "f(/*A*/a //x\\n,b , /*D*/ c)" $
        parse expression "" "f(/*A*/a //x\n,b , /*D*/ c)" @?=
        (Right $ Call (Identifier "f" [])
                      [ Identifier "a" [LineBased "x"]
                      , Identifier "b" []
                      , Identifier "c" []]
                      [[Bracketed "A"], [], [Bracketed "D"]] [])
    , testCase "M'" $ parse expression "" "M'" @?=
        (Right $ Transpose (Identifier "M" []) [])
    , testCase "M/*a*/'/*b*/" $ parse expression "" "M/*a*/'/*b*/" @?=
        (Right $ Transpose (Identifier "M" [Bracketed "a"]) [Bracketed "b"])
    , testCase "x[]" $ parse expression "" "x[]" @?=
        (Right $ Index id_x [] [[]] [])
    , testCase "x[/*A*/]" $ parse expression "" "x[/*A*/]" @?=
        (Right $ Index id_x [] [[Bracketed "A"]] [])
    , testCase "x[3]" $ parse expression "" "x[3]" @?=
        (Right $ Index id_x [lit_3] [[]] [])
    , testCase "x[2,1]" $ parse expression "" "x[3,1]" @?=
        (Right $ Index id_x [lit_3, lit_1] [[], []] [])
    , testCase "x [ 2 , 1 ]" $ parse expression "" "x [ 3 , 1 ]" @?=
        (Right $ Index id_x [lit_3, lit_1] [[], []] [])
    , testCase "x/*A*/[/*B*/a/*C*/,/*D*/b/*E*/]/*F*/" $
        parse expression "" "x/*A*/[/*B*/a/*C*/,/*D*/b/*E*/]/*F*/" @?=
        (Right $ Index (Identifier "x" [Bracketed "A"])
                       [ Identifier "a" [Bracketed "C"]
                       , Identifier "b" [Bracketed "E"]]
                       [[Bracketed "B"], [Bracketed "D"]] [Bracketed "F"])
    , testCase "x[a/*A*/:/*B*/b/*C*/]" $
        parse expression "" "x[a/*A*/:/*B*/b/*C*/]" @?=
        (Right $ Index id_x
            [ Range (Just $ Identifier "a" [Bracketed "A"])
                    [Bracketed "B"]
                    (Just $ Identifier "b" [Bracketed "C"])
            ] [[]] [])
    , testCase "x[a:b,p:q]" $
        parse expression "" "x[a:b,p:q]" @?=
        (Right $ Index id_x
            [ Range (Just $ id_a) [] (Just $ id_b)
            , Range (Just $ id_p) [] (Just $ id_q)
            ] [[], []] [])
    , testCase "x[a:b][p:q]" $
        parse expression "" "x[a:b][p:q]" @?=
        (Right $ Index
          (Index id_x [Range (Just $ id_a) [] (Just $ id_b)] [[]] [])
          [Range (Just $ id_p) [] (Just $ id_q)] [[]] [])
    , testCase "annotated x[a:b][p:q]" $
        parse expression ""
        "x/*A*/[/*B*/a/*C*/:/*D*/b/*E*/]/*F*/[/*G*/p/*H*/:/*I*/q/*J*/]/*K*/" @?=
        (Right $ Index
          (Index (Identifier "x" [Bracketed "A"])
                 [Range (Just $ Identifier "a" [Bracketed "C"])
                        [Bracketed "D"]
                        (Just $ Identifier "b" [Bracketed "E"])]
                 [[Bracketed "B"]] [Bracketed "F"])
          [Range (Just $ Identifier "p" [Bracketed "H"])
                 [Bracketed "I"]
                 (Just $ Identifier "q" [Bracketed "J"])]
          [[Bracketed "G"]] [Bracketed "K"])
    ]
  , testGroup "printables"
    [ testCase "a" $ parse printables "" "a" @?=
      (Right $ Printables [id_a] [[]])
    , testCase "a, \"xy\", b" $ parse printables "" "a, \"xy\", b" @?=
      (Right $ Printables [id_a, StringLiteral "xy" [], id_b]
               [[], [], []])
    , testCase "/*A*/a,/*B*/b" $ parse printables "" "/*A*/a,/*B*/b" @?=
      (Right $ Printables [id_a, id_b] [[Bracketed "A"], [Bracketed "B"]])
    ]
  , testGroup "range"
    [ testCase "range a" $ parse range "" "a" @?= (Right $ id_a)
    , testCase "range 1" $ parse range "" "1" @?= (Right $ lit_1)
    , testCase "range a:b" $ parse range "" "a:b" @?=
        (Right $ Range (Just id_a) [] (Just id_b))
    , testCase "range a:b" $ parse range "" "a:b" @?=
        (Right $ Range (Just id_a) [] (Just id_b))
    , testCase "range :b" $ parse range "" ":b" @?=
        (Right $ Range Nothing [] (Just id_b))
    , testCase "range a:" $ parse range "" "a:" @?=
        (Right $ Range (Just id_a) [] Nothing)
    , testCase "range ':'" $ parse range "" ":" @?=
        (Right $ Range Nothing [] Nothing)
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
    , expectParseFail codeAnnotations1 "'' fails" ""
    ]
  , testGroup "annotated"
    [ testCase "numLiteral 1 comment" $ parse numLiteral "" "1//abc" @?=
      (Right $ (NumLiteral "1" Nothing Nothing [LineBased "abc"]))
    , testCase "numLiteral 2 comments" $
      parse numLiteral "" "1 /* ab */ // xyz" @?=
      (Right $ (NumLiteral "1" Nothing Nothing
                           [Bracketed " ab ", LineBased " xyz"]))
    ]
  , testGroup "whitespace"
    [ testCase "''" $ parse whitespace "" "" @?= Right ""
    , testCase "'  '" $ parse whitespace "" "  " @?= Right "  "
    , testCase "'\\t  \\t'" $ parse whitespace "" "\t  \t" @?= Right "\t  \t"
    , testCase "no newline" $ parse whitespace "" " \n" @?= Right " "
    ]
  , testGroup "break"
    [ testCase "break;" $ parse statement "" "break;" @?=
      (Right $ Break [] [])
    , testCase "break/*A/*; /*B*/" $ parse statement "" "break/*A*/;/*B*/" @?=
      (Right $ Break [Bracketed "A"] [Bracketed "B"])
    ]
  , testGroup "continue"
    [ testCase "continue;" $ parse statement "" "continue;" @?=
      (Right $ Continue [] [])
    , testCase "continue/*A/*; /*B*/" $
      parse statement "" "continue/*A*/;/*B*/" @?=
      (Right $ Continue [Bracketed "A"] [Bracketed "B"])
    ]
  , testGroup "return"
    [ testCase "return;" $ parse statement "" "return;" @?=
      (Right $ Return [] Nothing [])
    , testCase "return/*A*/; /*B*/" $
      parse statement "" "return/*A*/;/*B*/" @?=
      (Right $ Return [Bracketed "A"] Nothing [Bracketed "B"])
    , testCase "return a;" $ parse statement "" "return a;" @?=
      (Right $ Return [] (Just id_a) [])
    , testCase "return/*A/*a;/*B*/" $
      parse statement "" "return/*A*/a;/*B*/" @?=
      (Right $ Return [Bracketed "A"] (Just id_a) [Bracketed "B"])
    ]
  , testGroup "block"
    [ testCase "{}" $ parse statement "" "{}" @?=
      (Right $ Block [] [] [])
    , testCase "{return; continue; }" $
      parse statement "" "{return; continue; }" @?=
      (Right $ Block [] [Return [] Nothing [], Continue [] []] [])
    , testCase "{/*A*/return; } /*B*/" $
      parse statement "" "{/*A*/return; } /*B*/" @?=
      (Right $ Block [Bracketed "A"] [Return [] Nothing []]
                     [Bracketed "B"])
    ]
{- ----STATEMENTS---- -}
  , testGroup "if/else"
    [ testCase "if (a) return;" $ parse statement "" "if (a) return;"
      @?= (Right $ If [] (Parens [] id_a []) (Return [] Nothing []))
    , testCase "if (a) return; else {}" $
      parse statement "" "if (a) return; else {}"
      @?= (Right $ IfElse [] (Parens [] id_a []) (Return [] Nothing []) []
                          (Block [] [] []))
    , testCase "if/*A*/(a) return; else/*B*/{}" $
      parse statement "" "if/*A*/(a) return; else/*B*/{}"
      @?= (Right $ IfElse [Bracketed "A"] (Parens [] id_a [])
                          (Return [] Nothing [])
                          [Bracketed "B"] (Block [] [] []))
    ]
  , testGroup "assignments"
    [ testCase "a=b;" $ parse statement "" "a=b;" @?=
      (Right $ Assign id_a [] id_b [])
    , testCase "a = b ;" $ parse statement "" "a = b ;" @?=
      (Right $ Assign id_a [] id_b [])
    , testCase "annotated assign" $
      parse statement "" "a /*A*/ =\nb/*B*/ ; /*C*/" @?=
      (Right $ Assign (Identifier "a" [Bracketed "A"]) [Newline]
                      (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a<-b;" $ parse statement "" "a<-b;" @?=
      (Right $ ArrowAssign id_a [] id_b [])
    , testCase "a <- b ;" $ parse statement "" "a <- b ;" @?=
      (Right $ ArrowAssign id_a [] id_b [])
    , testCase "annotated arrow assign" $
      parse statement "" "a /*A*/ <-\nb/*B*/ ; /*C*/" @?=
      (Right $ ArrowAssign (Identifier "a" [Bracketed "A"]) [Newline]
                           (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a+=b;" $ parse statement "" "a+=b;" @?=
      (Right $ PlusAssign id_a [] id_b [])
    , testCase "a += b ;" $ parse statement "" "a += b ;" @?=
      (Right $ PlusAssign id_a [] id_b [])
    , testCase "annotated plus assign" $
      parse statement "" "a /*A*/ +=\nb/*B*/ ; /*C*/" @?=
      (Right $ PlusAssign (Identifier "a" [Bracketed "A"]) [Newline]
                          (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a-=b;" $ parse statement "" "a-=b;" @?=
      (Right $ MinusAssign id_a [] id_b [])
    , testCase "a -= b ;" $ parse statement "" "a -= b ;" @?=
      (Right $ MinusAssign id_a [] id_b [])
    , testCase "annotated minus assign" $
      parse statement "" "a /*A*/ -=\nb/*B*/ ; /*C*/" @?=
      (Right $ MinusAssign (Identifier "a" [Bracketed "A"]) [Newline]
                           (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a*=b;" $ parse statement "" "a*=b;" @?=
      (Right $ TimesAssign id_a [] id_b [])
    , testCase "a *= b ;" $ parse statement "" "a *= b ;" @?=
      (Right $ TimesAssign id_a [] id_b [])
    , testCase "annotated times assign" $
      parse statement "" "a /*A*/ *=\nb/*B*/ ; /*C*/" @?=
      (Right $ TimesAssign (Identifier "a" [Bracketed "A"]) [Newline]
                           (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a/=b;" $ parse statement "" "a/=b;" @?=
      (Right $ DivideAssign id_a [] id_b [])
    , testCase "a /= b ;" $ parse statement "" "a /= b ;" @?=
      (Right $ DivideAssign id_a [] id_b [])
    , testCase "annotated div assign" $
      parse statement "" "a /*A*/ /=\nb/*B*/ ; /*C*/" @?=
      (Right $ DivideAssign (Identifier "a" [Bracketed "A"]) [Newline]
                            (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a.*=b;" $ parse statement "" "a.*=b;" @?=
      (Right $ EltTimesAssign id_a [] id_b [])
    , testCase "a .*= b ;" $ parse statement "" "a .*= b ;" @?=
      (Right $ EltTimesAssign id_a [] id_b [])
    , testCase "annotated elttimes assign" $
      parse statement "" "a /*A*/ .*=\nb/*B*/ ; /*C*/" @?=
      (Right $ EltTimesAssign (Identifier "a" [Bracketed "A"]) [Newline]
                           (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    , testCase "a./=b;" $ parse statement "" "a./=b;" @?=
      (Right $ EltDivideAssign id_a [] id_b [])
    , testCase "a ./= b ;" $ parse statement "" "a ./= b ;" @?=
      (Right $ EltDivideAssign id_a [] id_b [])
    , testCase "annotated eltdiv assign" $
      parse statement "" "a /*A*/ ./=\nb/*B*/ ; /*C*/" @?=
      (Right $ EltDivideAssign (Identifier "a" [Bracketed "A"]) [Newline]
                               (Identifier "b" [Bracketed "B"]) [Bracketed "C"])
    ]
  , testGroup "increment target"
    [ testCase "target += a;" $ parse statement "" "target += a;" @?=
      (Right $ TargetPlusAssign [] [] id_a [])
    , testCase "target/*A*/+=/*B*/a;/*C*/" $
      parse statement "" "target/*A*/+=/*B*/a;/*C*/" @?=
      (Right $ TargetPlusAssign [Bracketed "A"] [Bracketed "B"] id_a
                                [Bracketed "C"])
    ]
  , testGroup "increment_log_prob"
    [ testCase "increment_log_prob(a);" $
      parse statement "" "increment_log_prob(a);" @?=
      (Right $ IncrementLogProb [] [] id_a [] [])
    , testCase "increment_log_prob/*A*/(/*B*/a)/*C*/;/*D*/" $
      parse statement "" "increment_log_prob/*A*/(/*B*/a)/*C*/;/*D*/" @?=
      (Right $ IncrementLogProb [Bracketed "A"] [Bracketed "B"] id_a
                                [Bracketed "C"] [Bracketed "D"])
    ]
  , testGroup "for loop"
    [ testCase "for (x in a) return;" $
      parse statement "" "for (x in a) return;" @?=
      (Right $ For [] [] id_x [] id_a [] (Return [] Nothing []))
    , testCase "annotated For" $
      parse statement "" "for/*A*/(/*B*/x in/*C*/ a)/*D*/ return;" @?=
      (Right $ For [Bracketed "A"] [Bracketed "B"] id_x [Bracketed "C"] id_a
                   [Bracketed "D"]  (Return [] Nothing []))
    , testCase "for (x in 1:3) return;" $
      parse statement "" "for (x in 1:3) return;" @?=
      (Right $ ForRange [] [] id_x [] lit_1 [] lit_3 [] (Return [] Nothing []))
    , testCase "annotated ForRange" $
      parse statement "" "for/*A*/(/*B*/x in/*C*/ a:/*D*/b)/*E*/ return;" @?=
      (Right $ ForRange [Bracketed "A"] [Bracketed "B"] id_x [Bracketed "C"]
                        id_a [Bracketed "D"] id_b [Bracketed "E"]
                        (Return [] Nothing []))
    ]
  , testGroup "while loop"
    [ testCase "while (a) return;" $ parse statement "" "while (a) return;" @?=
      (Right $ While [] [] id_a [] (Return [] Nothing []))
    , testCase "while/*A*/(a) return;" $
      parse statement "" "while/*A*/(/*B*/a)/*C*/ return;"
      @?= (Right $ While [Bracketed "A"] [Bracketed "B"] id_a [Bracketed "C"]
                         (Return [] Nothing []))
    ]
  , testGroup "tilde stmt"
    [ testCase "a+1 ~ x(p,q);" $ parse statement "" "a+1 ~ x(p, q);" @?=
      (Right $ Tilde (Add id_a [] lit_1) []
                     (Call id_x [id_p, id_q] [[], []] []) [])
    , testCase "a ~/*A*/x();/*B*/" $ parse statement "" "a ~/*A*/x();/*B*/" @?=
      (Right $ Tilde id_a [Bracketed "A"] (Call id_x [] [[]] [])
                     [Bracketed "B"])
    ]
  , testGroup "print"
    [ testCase "print(a,1);" $ parse statement "" "print(a,1);" @?=
      (Right $ Print [] (Printables [id_a, lit_1] [[], []]) [] [])
    , testCase "print/*A*/(a,1)/*B*/;/*C*/" $
      parse statement "" "print/*A*/(a,1)/*B*/;/*C*/" @?=
      (Right $ Print [Bracketed "A"] (Printables [id_a, lit_1] [[], []])
                     [Bracketed "B"] [Bracketed "C"])
    , expectParseFail statement "print(); fails" "print();"
    ]
  , testGroup "reject"
    [ testCase "reject(a,1);" $ parse statement "" "reject(a,1);" @?=
      (Right $ Reject [] (Printables [id_a, lit_1] [[], []]) [] [])
    , testCase "reject/*A*/(a,1)/*B*/;/*C*/" $
      parse statement "" "reject/*A*/(a,1)/*B*/;/*C*/" @?=
      (Right $ Reject [Bracketed "A"] (Printables [id_a, lit_1] [[], []])
                      [Bracketed "B"] [Bracketed "C"])
    , expectParseFail statement "reject(); fails" "reject();"
    ]
  , testGroup "empty stmt"
    [ testCase ";" $ parse statement "" ";" @?= (Right $ Empty [])
    , testCase ";/*A*/" $ parse statement "" ";/*A*/" @?=
      (Right $ Empty [Bracketed "A"])
    ]
  , testGroup "special cases"
    [ testCase "returnn = a;" $ parse statement "" "returnn = a;" @?=
      (Right $ Assign (Identifier "returnn" []) [] id_a [])
    , testCase "break_ = a;" $ parse statement "" "break_ = a;" @?=
      (Right $ Assign (Identifier "break_" []) [] id_a [])
    , testCase "continue_ = a;" $ parse statement "" "continue_ = a;" @?=
      (Right $ Assign (Identifier "continue_" []) [] id_a [])
    , testCase "if_ = a;" $ parse statement "" "if_ = a;" @?=
      (Right $ Assign (Identifier "if_" []) [] id_a [])
    , testCase "else_ = a;" $ parse statement "" "else_ = a;" @?=
      (Right $ Assign (Identifier "else_" []) [] id_a [])
    ]
  , testGroup "top var types"
    [ testCase "int" $ parse (varType True) "" "int" @?=
      (Right $ Int [] Nothing)
    , testCase "real" $ parse (varType True) "" "real" @?=
      (Right $ Real [] Nothing)
    , testCase "real'" $ parse (varType False) "" "real" @?=
      (Right $ Real [] Nothing)
    , testCase "complex" $ parse (varType True) "" "complex" @?=
      (Right $ Complex [] Nothing)
    , testCase "int/*A*/" $ parse (varType True) "" "int/*A*/" @?=
      (Right $ Int [Bracketed "A"] Nothing)
    , testCase "real/*A*/" $ parse (varType True) "" "real/*A*/" @?=
      (Right $ Real [Bracketed "A"] Nothing)
    , testCase "complex/*A*/" $ parse (varType True) "" "complex/*A*/" @?=
      (Right $ Complex [Bracketed "A"] Nothing)
    , testCase "int<upper=a,lower=b>" $
      parse (varType True) "" "int<upper=a,lower=b>"
      @?= (Right $ Int [] (Just $ VarConstraints [ Upper [] [] [] id_a
                                                 , Lower [] [] [] id_b]
                                                 []))
    , testCase "int<lower=a,upper=b>" $
      parse (varType True) "" "int<lower=a,upper=b>"
      @?= (Right $ Int [] (Just $ VarConstraints [ Lower [] [] [] id_a
                                                 , Upper [] [] [] id_b]
                                                 []))
    , testCase "real<offset=b>" $
      parse (varType True) "" "int<offset=b>"
      @?= (Right $ Int [] (Just $ VarConstraints [Offset [] [] [] id_b] []))
    , testCase "annotated real" $
      parse (varType True) ""
      "real/*A*/</*B*/lower/*C*/=/*D*/a/*E*/,/*F*/upper/*G*/=/*H*/b/*I*/>/*J*/"
      @?= (Right $ Real [Bracketed "A"]
                        (Just $ VarConstraints
                                 [ Lower [Bracketed "B"] [Bracketed "C"]
                                         [Bracketed "D"]
                                         (Identifier "a" [Bracketed "E"])
                                 , Upper [Bracketed "F"] [Bracketed "G"]
                                         [Bracketed "H"]
                                         (Identifier "b" [Bracketed "I"])
                                 ]
                                 [Bracketed "J"]))
    ]
  , testGroup "top var declarations"
    [ testCase "int x = a;" $
      parse (varDeclaration True True) "" "int x = a;" @?=
      (Right $ VarDeclAssign (Int [] Nothing) id_x  [] id_a [])
    , testCase "real x = 1;" $
      parse (varDeclaration True True) "" "real x = 1;" @?=
      (Right $ VarDeclAssign (Real [] Nothing) id_x [] lit_1 [])
    , testCase "real x;" $
      parse (varDeclaration True True) "" "real x;" @?=
      (Right $ VarDecl (Real [] Nothing) id_x [])
    , testCase "annotated vec" $
      parse (varDeclaration True True) ""
      "vector/*A*/[/*B*/3]/*C*/ x =/*D*/2;/*E*/" @?=
      (Right $ VarDeclAssign
                 (Vector [Bracketed "A"] Nothing [Bracketed "B"] lit_3
                         [Bracketed "C"])
                 id_x [Bracketed "D"] lit_2 [Bracketed "E"])
    , testCase "annotated mat" $
      parse (varDeclaration True True) ""
      "matrix/*A*/[/*B*/3,/*C*/1]/*D*/ x =/*E*/2;/*F*/" @?=
      (Right $ VarDeclAssign
                 (Matrix [Bracketed "A"] Nothing [Bracketed "B"] lit_3
                         [Bracketed "C"] lit_1 [Bracketed "D"])
                 id_x [Bracketed "E"] lit_2 [Bracketed "F"])
    , testCase "cholesky_factor_mat" $
      parse (varDeclaration True True) "" "cholesky_factor_cov[p] x;" @?=
      (Right $ VarDecl (CholeskyFactorCov [] [] id_p Nothing []) id_x [])
    , testCase "cholesky_factor_mat'" $
      parse (varDeclaration True True) "" "cholesky_factor_cov[p,q] x;" @?=
      (Right $ VarDecl (CholeskyFactorCov [] [] id_p
                           (Just $ ([], id_q)) []) id_x [])
    ]
  , testGroup "top var declarations fail"
    [ expectParseFail (varDeclaration False True) "i)" "int<lower=0> i;"
    , expectParseFail (varDeclaration True False) "i)" "int<lower=0> i = 0;"
    , expectParseFail (varDeclaration True True) "i)" "real<> i;"
    ]
  ]


{- definitions of expressions, for readability -}
id_a :: Expr
id_a = Identifier "a" []

id_b :: Expr
id_b = Identifier "b" []

id_c :: Expr
id_c = Identifier "c" []

id_x :: Expr
id_x = Identifier "x" []

id_p :: Expr
id_p = Identifier "p" []

id_q :: Expr
id_q = Identifier "q" []

lit_0 :: Expr
lit_0 = NumLiteral "0" Nothing Nothing []

lit_1 :: Expr
lit_1 = NumLiteral "1" Nothing Nothing []

lit_2 :: Expr
lit_2 = NumLiteral "2" Nothing Nothing []

lit_3 :: Expr
lit_3 = NumLiteral "3" Nothing Nothing []
