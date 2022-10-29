{-# LANGUAGE OverloadedStrings #-}
module Tests.Parser (tests) where

import Data.Either      (isLeft)
import Data.Text        (Text, unpack)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Parsec      (parse)
import Text.Parsec.Text (Parser)

import Text.Nast.AnnotatedAST
import Text.Nast.Parser


-- | Assert that a parse fails (returns Left)
assertParseFail :: Parser a  -- ^ parser to use
                -> String    -- ^ test case name
                -> Text      -- ^ source to parse
                -> TestTree
assertParseFail p t s = testCase t $ assertBool "" $ isLeft $ parse p "" s

-- | Test parser with test case name
testParserMsg :: (Show a, Eq a) => Parser a  -- ^ parser to test
                                -> String    -- ^ test case name
                                -> Text      -- ^ source to parse
                                -> a         -- ^ expected value
                                -> TestTree
testParserMsg p t s e = testCase t $ parse p "" s @?= (Right $ e)

-- | Test parser with test case name equal to source
testParser:: (Show a, Eq a) => Parser a  -- ^ parser to test
                            -> Text      -- ^ source to parse
                            -> a         -- ^ expected value
                            -> TestTree
testParser p s = testParserMsg p (unpack s) s

testExprMsg :: String -> Text -> Expr -> TestTree
testExprMsg = testParserMsg expression

testExpr :: Text -> Expr -> TestTree
testExpr = testParser expression

testStmtMsg :: String -> Text -> Stmt -> TestTree
testStmtMsg = testParserMsg statement

testStmt :: Text -> Stmt -> TestTree
testStmt = testParser statement

tests :: [TestTree]
tests =
  [ testGroup "Precedence 10"
    [ testExpr "a ? b : c" $ Conditional id_a [] id_b [] id_c
    , testExpr "a?b:c" $ Conditional id_a [] id_b [] id_c
    , testExprMsg "associativity" "a?b:c?p:q" $
      Conditional id_a [] id_b [] (Conditional id_c [] id_p [] id_q)
    ]
  , testGroup "Precedence 9"
    [ testExpr "a || b||c" $ Binary (Binary id_a Or [] id_b) Or [] id_c
    , testExpr "a|| b ||c" $ Binary (Binary id_a Or [] id_b) Or [] id_c
    , testExprMsg "annotated ||" "x /*a*/ || /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) Or
             [Bracketed "b", Bracketed "c", Newline]
             (Identifier "y" [Bracketed "d"])
    ]
  , testGroup "Precedence 8"
    [ testExpr "a && b&&c" $ Binary (Binary id_a And [] id_b) And [] id_c
    , testExpr "a&& b &&c" $ Binary (Binary id_a And [] id_b) And [] id_c
    , testExprMsg "annotated &&" "x /*a*/ && /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) And
             [Bracketed "b", Bracketed "c", Newline]
             (Identifier "y" [Bracketed "d"])
    ]
  , testGroup "Precedence 7"
    [ testExpr "a == b==c" $ Binary (Binary id_a Equal [] id_b) Equal [] id_c
    , testExpr "a != b!=c" $
        Binary (Binary id_a NotEqual [] id_b) NotEqual [] id_c
    , testExpr "a != b==c" $ Binary (Binary id_a NotEqual [] id_b) Equal [] id_c
    , testExpr "a== b !=c" $ Binary (Binary id_a Equal [] id_b) NotEqual [] id_c
    , testExprMsg "annotated ==" "x /*a*/ == /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) Equal
             [Bracketed "b", Bracketed "c", Newline]
             (Identifier "y" [Bracketed "d"])
    , testExprMsg "annotated !=" "x /*a*/ != /*b*/ /*c*/\ny /*d*/" $
        Binary (Identifier "x" [Bracketed "a"]) NotEqual
               [Bracketed "b", Bracketed "c", Newline]
               (Identifier "y" [Bracketed "d"])
    ]
  , testGroup "Precedence 6"
    [ testExpr "a > b > c" $ Binary (Binary id_a Gt [] id_b) Gt [] id_c
    , testExpr "a >= b >= c" $ Binary (Binary id_a Geq [] id_b) Geq [] id_c
    , testExpr "a < b < c" $ Binary (Binary id_a Lt [] id_b) Lt [] id_c
    , testExpr "a <= b <= c" $ Binary (Binary id_a Leq [] id_b) Leq [] id_c
    , testExprMsg "annotated Gt" "x /*a*/ > /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) Gt
             [Bracketed "b", Bracketed "c", Newline]
             (Identifier "y" [Bracketed "d"])
    , testExprMsg "annotated Geq" "x /*a*/ >= /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) Geq
             [Bracketed "b", Bracketed "c", Newline]
             (Identifier "y" [Bracketed "d"])
    , testExprMsg "annotated Lt" "x /*a*/ < /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) Lt
            [Bracketed "b", Bracketed "c", Newline]
            (Identifier "y" [Bracketed "d"])
    , testExprMsg "annotated Leq" "x /*a*/ <= /*b*/ /*c*/\ny /*d*/" $
      Binary (Identifier "x" [Bracketed "a"]) Leq
             [Bracketed "b", Bracketed "c", Newline]
             (Identifier "y" [Bracketed "d"])
    ]
  , testGroup "Precedence 5"
    [ testExpr "p + q" $ Binary id_p Add [] id_q
    , testExpr "0+1" $ Binary lit_0 Add [] lit_1
    , testExprMsg "annotated addition" "3 /*a*/ + /*b*/ /*c*/\n1 /*d*/" $
      Binary (NumLiteral "3" Nothing Nothing [Bracketed "a"]) Add 
             [Bracketed "b", Bracketed "c", Newline]
             (NumLiteral "1" Nothing Nothing [Bracketed "d"])
    , testExpr "1 +2 +  3" $ Binary (Binary lit_1 Add [] lit_2) Add [] lit_3
    , testExpr "3 - 4" $ Binary lit_3 Sub [] (NumLiteral "4" Nothing Nothing [])
    , testExpr "0-1" $ Binary lit_0 Sub [] lit_1
    , testExprMsg "annotated subtraction" "3 /*a*/ - /*b*/ /*c*/\n2 /*d*/" $
      Binary (NumLiteral "3" Nothing Nothing [Bracketed "a"]) Sub
             [Bracketed "b", Bracketed "c", Newline]
             (NumLiteral "2" Nothing Nothing [Bracketed "d"])
    , testExpr "1 -2 +  3" $ Binary (Binary lit_1 Sub [] lit_2) Add [] lit_3
    ]
  , testGroup "Precedence 4"
    [ testExpr "3 * x" $ Binary lit_3 Mul [] id_x
    , testExpr "0*1" $ Binary lit_0 Mul [] lit_1
    , testExprMsg "annotated multiplication" "3 /*a*/ * /*b*/ /*c*/\n1 /*d*/" $
      Binary (NumLiteral "3" Nothing Nothing [Bracketed "a"]) Mul
             [Bracketed "b", Bracketed "c", Newline]
             (NumLiteral "1" Nothing Nothing [Bracketed "d"])
    , testExpr "a * b + c" $ Binary (Binary id_a Mul [] id_b) Add [] id_c
    , testExpr "a + b / c" $ Binary id_a Add [] (Binary id_b Div [] id_c)
    , testExpr "p .* q" $ Binary id_p EltMul [] id_q
    , testExpr "p ./ q" $ Binary id_p EltDiv [] id_q
    ]
  , testGroup "Precedence 3"
    [ testExpr "3 \\ x" $ Binary lit_3 LDiv [] id_x
    , testExpr "0%\\%1" $ Binary lit_0 IntDiv [] lit_1
    , testExprMsg "annotated int div" "3 /*a*/ %\\% /*b*/ /*c*/\n1 /*d*/" $
      Binary (NumLiteral "3" Nothing Nothing [Bracketed "a"]) IntDiv
             [Bracketed "b", Bracketed "c", Newline]
             (NumLiteral "1" Nothing Nothing [Bracketed "d"])
    , testExpr "a \\ b + c" $ Binary (Binary id_a LDiv [] id_b) Add [] id_c
    , testExpr "a + b %\\% c" $ Binary id_a Add [] (Binary id_b IntDiv [] id_c)
    ]
  , testGroup "Precedence 3"
    [ testExpr "3^x" $ Binary lit_3 Pow [] id_x
    , testExpr "x^p^q" $ Binary id_x Pow [] (Binary id_p Pow [] id_q)
    , testExpr "x.^p.^q" $ Binary id_x EltPow [] (Binary id_p EltPow [] id_q)
    , testExpr "p .^ 1" $ Binary id_p EltPow [] lit_1
    , testExpr "p.^ 1" $ Binary id_p EltPow [] lit_1
    , testExpr "a .^2" $ Binary id_a EltPow [] lit_2
    , testExpr "a.^2" $ Binary id_a EltPow [] lit_2
    , testExprMsg "annotated pow" "3 /*a*/ ^ /*b*/ /*c*/\n1 /*d*/" $
      Binary (NumLiteral "3" Nothing Nothing [Bracketed "a"]) Pow
             [Bracketed "b", Bracketed "c", Newline]
             (NumLiteral "1" Nothing Nothing [Bracketed "d"])
    , testExpr "a ^ b + c" $ Binary (Binary id_a Pow [] id_b) Add [] id_c
    , testExpr "a + b .^ c" $ Binary id_a Add [] (Binary id_b EltPow [] id_c)
    ]
  , testGroup "Precedence 2"
    [ testExpr "!a" $ LogicalNeg [] id_a
    , testExpr "+a" $ UnaryPlus [] id_a
    , testExpr "-a" $ UnaryMinus [] id_a
    , testExpr "-!+a" $ UnaryMinus [] $ LogicalNeg [] $ UnaryPlus [] id_a
    , testExpr "+ /*xy*/ p //uv" $
      UnaryPlus [Bracketed "xy"] (Identifier "p" [LineBased "uv"])
    , testExpr "- /*xy*/ p //uv" $
      UnaryMinus [Bracketed "xy"] (Identifier "p" [LineBased "uv"])
    , testExpr "! /*xy*/ p //uv" $
      LogicalNeg [Bracketed "xy"] (Identifier "p" [LineBased "uv"])
    ]
  , testGroup "lhs"
    [ testParser lhs "xyz" $ Identifier "xyz" []
    , testParser lhs "x[3]" $ Index id_x [lit_3] [[]] []
    , testParser lhs "x[3,1]" $ Index id_x [lit_3, lit_1] [[], []] []
    , assertParseFail lhs "'-b' fails" "-b"
    ]
  , testGroup "numeric literal"
    [ testParser numLiteral "23" $ NumLiteral "23" Nothing Nothing []
    , testParser numLiteral "-7" $ NumLiteral "-7" Nothing Nothing []
    , testParser numLiteral "0" $ NumLiteral "0" Nothing Nothing []
    , testParser numLiteral "12.23" $ NumLiteral "12" (Just "23") Nothing []
    , testParser numLiteral "03.14" $ NumLiteral "03" (Just "14") Nothing []
    , testParser numLiteral "-13.1e07" $
        NumLiteral "-13" (Just "1") (Just "07") []
    , testParser numLiteral "3e6" $ NumLiteral "3" Nothing (Just "6") []
    , testParser numLiteral "-98e-012" $
        NumLiteral "-98" Nothing (Just "-012") []
    , testParser numLiteral "003.001e006" $
        NumLiteral "003" (Just "001") (Just "006") []
    ]
  , testGroup "string literals"
    [ testParserMsg stringLiteral "'' (empty string)" "\"\"" $
        StringLiteral "" []
    , testParserMsg stringLiteral "'abc'" "\"abc\"" $
        StringLiteral "abc" []
    , assertParseFail stringLiteral "'a\\na' fails" "\"a\na\""
    ]
  , testGroup "parentheses"
    [ testExpr "(3)" $ Parens [] (NumLiteral "3" Nothing Nothing []) []
    , testExprMsg "(//ab\\n 1 ) /*xy*/" "(//ab\n 1 ) /*xy*/" $
        Parens [LineBased "ab"] lit_1 [Bracketed "xy"]
    , testExprMsg "(\\n3)" "(\n3)" $ Parens [Newline] lit_3 []
    , testExprMsg "(/*a*/ /*b*/\\n3)" "(/*a*/ /*b*/\n3)" $
        Parens [Bracketed "a", Bracketed "b", Newline] lit_3 []
    ]
  , testGroup "identifier"
    [ testExpr "a" $ Identifier "a" []
    , testExpr "xyz" $ Identifier "xyz" []
    , testExpr "xyZz12" $ Identifier "xyZz12" []
    , assertParseFail identifier "fail i)" "1"
    , assertParseFail identifier "fail ii)" "12ab"
    , assertParseFail identifier "fail iii)" "_xy"
    , assertParseFail identifier "fail iv)" "ab__"
    ]
  , testGroup "Precedence 0"
    [ testExpr "abc()" $
        Call (Identifier "abc" []) [] [[]] []
    , testExpr "f(/*xy*/)" $
        Call (Identifier "f" []) [] [[Bracketed "xy"]] []
    , testExpr "f(a, b , c)" $
        Call (Identifier "f" [])
                      [ Identifier "a" []
                      , Identifier "b" []
                      , Identifier "c" []]
                      [[], [], []] []
    , testExpr "f /*ab*/ ( /*xy*/) /*pq*/" $
        Call (Identifier "f" [Bracketed "ab"]) [] [[Bracketed "xy"]]
             [Bracketed "pq"]
    , testExprMsg "f(/*A*/a //x\\n,b , /*D*/ c)"
                  "f(/*A*/a //x\n,b , /*D*/ c)" $
        Call (Identifier "f" [])
             [ Identifier "a" [LineBased "x"]
             , Identifier "b" []
             , Identifier "c" []]
             [[Bracketed "A"], [], [Bracketed "D"]] []
    , testExpr "M'" $ Transpose (Identifier "M" []) []
    , testExpr "M/*a*/'/*b*/" $
        Transpose (Identifier "M" [Bracketed "a"]) [Bracketed "b"]
    , testExpr "x[]" $ Index id_x [] [[]] []
    , testExpr "x[/*A*/]" $ Index id_x [] [[Bracketed "A"]] []
    , testExpr "x[3]" $ Index id_x [lit_3] [[]] []
    , testExpr "x[2,1]" $ Index id_x [lit_2, lit_1] [[], []] []
    , testExpr "x [ 2 , 1 ]" $ Index id_x [lit_2, lit_1] [[], []] []
    , testExpr "x/*A*/[/*B*/a/*C*/,/*D*/b/*E*/]/*F*/" $
        Index (Identifier "x" [Bracketed "A"])
              [ Identifier "a" [Bracketed "C"] , Identifier "b" [Bracketed "E"]]
              [[Bracketed "B"], [Bracketed "D"]] [Bracketed "F"]
    , testExpr "x[a/*A*/:/*B*/b/*C*/]" $
        Index id_x [ Range (Just $ Identifier "a" [Bracketed "A"])
                       [Bracketed "B"]
                       (Just $ Identifier "b" [Bracketed "C"])
                   ] [[]] []
    , testExpr "x[a:b,p:q]" $
        Index id_x [ Range (Just $ id_a) [] (Just $ id_b)
                   , Range (Just $ id_p) [] (Just $ id_q)
                   ] [[], []] []
    , testExpr "x[a:b][p:q]" $
        Index (Index id_x [Range (Just $ id_a) [] (Just $ id_b)] [[]] [])
              [Range (Just $ id_p) [] (Just $ id_q)] [[]] []
    , testExprMsg "annotated x[a:b][p:q]"
        "x/*A*/[/*B*/a/*C*/:/*D*/b/*E*/]/*F*/[/*G*/p/*H*/:/*I*/q/*J*/]/*K*/" $
        Index
          (Index (Identifier "x" [Bracketed "A"])
                 [Range (Just $ Identifier "a" [Bracketed "C"])
                        [Bracketed "D"]
                        (Just $ Identifier "b" [Bracketed "E"])]
                 [[Bracketed "B"]] [Bracketed "F"])
          [Range (Just $ Identifier "p" [Bracketed "H"])
                 [Bracketed "I"]
                 (Just $ Identifier "q" [Bracketed "J"])]
          [[Bracketed "G"]] [Bracketed "K"]
    ]
  , testGroup "printables"
    [ testParser printables "a" $ Printables [id_a] [[]]
    , testParser printables "a, \"xy\", b" $
        Printables [id_a, StringLiteral "xy" [], id_b] [[], [], []]
    , testParser printables "/*A*/a,/*B*/b" $
        Printables [id_a, id_b] [[Bracketed "A"], [Bracketed "B"]]
    ]
  , testGroup "range"
    [ testParserMsg range "range a" "a" $ id_a
    , testParserMsg range "range 1" "1" $ lit_1
    , testParserMsg range "range a:b" "a:b" $ Range (Just id_a) [] (Just id_b)
    , testParserMsg range "range a:b" "a:b" $ Range (Just id_a) [] (Just id_b)
    , testParserMsg range "range :b" ":b" $ Range Nothing [] (Just id_b)
    , testParserMsg range "range a:" "a:" $ Range (Just id_a) [] Nothing
    , testParserMsg range "range ':'" ":" $ Range Nothing [] Nothing
    ]
  , testGroup "codeAnnotations"
    [ testParser codeAnnotations "//" [LineBased ""]
    , testParser codeAnnotations "// abc" [LineBased " abc"]
    , testParser codeAnnotations "/**/" [Bracketed ""]
    , testParser codeAnnotations "/* abc */" [Bracketed " abc "]
    , testParser codeAnnotations "/* ab*c*/" [Bracketed " ab*c"]
    , testParserMsg codeAnnotations "/* a\\nb*c*/\\n//xy\\n//pq"
                                    "/* a\nb*c*/\n//xy\n//pq"
        [Bracketed " a\nb*c", Newline, LineBased "xy", LineBased "pq"]
    , testParserMsg codeAnnotations "\\n" "\n" [Newline]
    , testParserMsg codeAnnotations "' '" " " []
    , testParserMsg codeAnnotations "''" " " []
    ]
  , testGroup "codeAnnotations1"
    [ testParser codeAnnotations1 "//" $ [LineBased ""]
    , testParser codeAnnotations1 "// abc" $ [LineBased " abc"]
    , testParser codeAnnotations1 "/**/" $ [Bracketed ""]
    , testParser codeAnnotations1 "/* abc */" $ [Bracketed " abc "]
    , testParser codeAnnotations1 "/* ab*c*/" $ [Bracketed " ab*c"]
    , testParserMsg codeAnnotations1 "/* a\\nb*c*/\\n//xy\\n//pq"
                                     "/* a\nb*c*/\n//xy\n//pq"
        [Bracketed " a\nb*c", Newline, LineBased "xy", LineBased "pq"]
    , testParserMsg codeAnnotations1 "\\n" "\n" [Newline]
    , testParserMsg codeAnnotations1 "' '" " " []
    , assertParseFail codeAnnotations1 "'' fails" ""
    ]
  , testGroup "annotated"
    [ testParserMsg numLiteral "numLiteral 1 comment"  "1//abc" $
        NumLiteral "1" Nothing Nothing [LineBased "abc"]
    , testParserMsg numLiteral "numLiteral 2 comments" "1 /* ab */ // xyz" $
        NumLiteral "1" Nothing Nothing [Bracketed " ab ", LineBased " xyz"]
    ]
  , testGroup "whitespace"
    [ testParserMsg whitespace "''" "" ""
    , testParserMsg whitespace "'  '" "  " "  "
    , testParserMsg whitespace "'\\t  \\t'" "\t  \t" "\t  \t"
    , testParserMsg whitespace "no newline" " \n" " "
    ]
{- ----STATEMENTS---- -}
  , testGroup "break"
    [ testStmt "break;" $ Break [] []
    , testStmt "break/*A*/; /*B*/" $ Break [Bracketed "A"] [Bracketed "B"]
    ]
  , testGroup "continue"
    [ testStmt "continue;" $ Continue [] []
    , testStmt "continue/*A*/; /*B*/" $ Continue [Bracketed "A"] [Bracketed "B"]
    ]
  , testGroup "return"
    [ testStmt "return;" $ Return [] Nothing []
    , testStmt "return/*A*/; /*B*/" $
        Return [Bracketed "A"] Nothing [Bracketed "B"]
    , testStmt "return a;" $ Return [] (Just id_a) []
    , testStmt "return/*A*/a;/*B*/" $
        Return [Bracketed "A"] (Just id_a) [Bracketed "B"]
    ]
  , testGroup "block"
    [ testStmt "{}" $ Block [] [] []
    , testStmt "{return; continue; }" $
        Block [] [Return [] Nothing [], Continue [] []] []
    , testStmt "{/*A*/return; } /*B*/" $
        Block [Bracketed "A"] [Return [] Nothing []] [Bracketed "B"]
    ]
  , testGroup "if/else"
    [ testStmt "if (a) return;" $
        If [] (Parens [] id_a []) (Return [] Nothing [])
    , testStmt "if (a) return; else {}" $
        IfElse [] (Parens [] id_a []) (Return [] Nothing []) [] (Block [] [] [])
    , testStmt "if/*A*/(a) return; else/*B*/{}" $
        IfElse [Bracketed "A"] (Parens [] id_a []) (Return [] Nothing [])
               [Bracketed "B"] (Block [] [] [])
    ]
  , testGroup "assignments"
    [ testStmt "a=b;" $ Assign id_a [] id_b []
    , testStmt "a = b ;" $ Assign id_a [] id_b []
    , testStmtMsg "annotated assign" "a /*A*/ =\nb/*B*/ ; /*C*/" $
        Assign (Identifier "a" [Bracketed "A"]) [Newline]
               (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a<-b;" $ ArrowAssign id_a [] id_b []
    , testStmt "a <- b ;" $ ArrowAssign id_a [] id_b []
    , testStmtMsg "annotated arrow assign" "a /*A*/ <-\nb/*B*/ ; /*C*/" $
        ArrowAssign (Identifier "a" [Bracketed "A"]) [Newline]
                    (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a+=b;" $ PlusAssign id_a [] id_b []
    , testStmt "a += b ;" $ PlusAssign id_a [] id_b []
    , testStmtMsg "annotated plus assign" "a /*A*/ +=\nb/*B*/ ; /*C*/" $
        PlusAssign (Identifier "a" [Bracketed "A"]) [Newline]
                   (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a-=b;" $ MinusAssign id_a [] id_b []
    , testStmt "a -= b ;" $ MinusAssign id_a [] id_b []
    , testStmtMsg "annotated minus assign" "a /*A*/ -=\nb/*B*/ ; /*C*/" $
        MinusAssign (Identifier "a" [Bracketed "A"]) [Newline]
                    (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a*=b;" $ TimesAssign id_a [] id_b []
    , testStmt "a *= b ;" $ TimesAssign id_a [] id_b []
    , testStmtMsg "annotated times assign" "a /*A*/ *=\nb/*B*/ ; /*C*/" $
      TimesAssign (Identifier "a" [Bracketed "A"]) [Newline]
                  (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a/=b;" $ DivideAssign id_a [] id_b []
    , testStmt "a /= b ;" $ DivideAssign id_a [] id_b []
    , testStmtMsg "annotated div assign" "a /*A*/ /=\nb/*B*/ ; /*C*/" $
        DivideAssign (Identifier "a" [Bracketed "A"]) [Newline]
                     (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a.*=b;" $ EltTimesAssign id_a [] id_b []
    , testStmt "a .*= b ;" $ EltTimesAssign id_a [] id_b []
    , testStmtMsg "annotated elttimes assign" "a /*A*/ .*=\nb/*B*/ ; /*C*/" $
        EltTimesAssign (Identifier "a" [Bracketed "A"]) [Newline]
                       (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    , testStmt "a./=b;" $ EltDivideAssign id_a [] id_b []
    , testStmt "a ./= b ;" $ EltDivideAssign id_a [] id_b []
    , testStmtMsg "annotated eltdiv assign" "a /*A*/ ./=\nb/*B*/ ; /*C*/" $
        EltDivideAssign (Identifier "a" [Bracketed "A"]) [Newline]
                        (Identifier "b" [Bracketed "B"]) [Bracketed "C"]
    ]
  , testGroup "increment target"
    [ testStmt "target += a;" $ TargetPlusAssign [] [] id_a []
    , testStmt "target/*A*/+=/*B*/a;/*C*/" $
        TargetPlusAssign [Bracketed "A"] [Bracketed "B"] id_a [Bracketed "C"]
    ]
  , testGroup "increment_log_prob"
    [ testStmt "increment_log_prob(a);" $
        IncrementLogProb [] [] id_a [] []
    , testStmt "increment_log_prob/*A*/(/*B*/a)/*C*/;/*D*/" $
        IncrementLogProb [Bracketed "A"] [Bracketed "B"] id_a [Bracketed "C"]
                         [Bracketed "D"]
    ]
  , testGroup "for loop"
    [ testStmt "for (x in a) return;" $
        For [] [] id_x [] id_a [] (Return [] Nothing [])
    , testStmtMsg "annotated For" "for/*A*/(/*B*/x in/*C*/ a)/*D*/ return;" $
        For [Bracketed "A"] [Bracketed "B"] id_x [Bracketed "C"] id_a
            [Bracketed "D"] (Return [] Nothing [])
    , testStmt "for (x in 1:3) return;" $
        ForRange [] [] id_x [] lit_1 [] lit_3 [] (Return [] Nothing [])
    , testStmtMsg "annotated ForRange"
        "for/*A*/(/*B*/x in/*C*/ a:/*D*/b)/*E*/ return;" $
        ForRange [Bracketed "A"] [Bracketed "B"] id_x [Bracketed "C"]
                 id_a [Bracketed "D"] id_b [Bracketed "E"]
                 (Return [] Nothing [])
    ]
  , testGroup "while loop"
    [ testStmt "while (a) return;" $ While [] [] id_a [] (Return [] Nothing [])
    , testStmt "while/*A*/(/*B*/a)/*C*/ return;" $
        While [Bracketed "A"] [Bracketed "B"] id_a [Bracketed "C"]
              (Return [] Nothing [])
    ]
  , testGroup "tilde stmt"
    [ testStmt "a+1 ~ x(p,q);" $
        Tilde (Binary id_a Add [] lit_1) [] (Call id_x [id_p, id_q] [[], []] []) []
    , testStmt "a ~/*A*/x();/*B*/" $
        Tilde id_a [Bracketed "A"] (Call id_x [] [[]] []) [Bracketed "B"]
    ]
  , testGroup "print"
    [ testStmt "print(a,1);" $
        Print [] (Printables [id_a, lit_1] [[], []]) [] []
    , testStmt "print/*A*/(a,1)/*B*/;/*C*/" $
        Print [Bracketed "A"] (Printables [id_a, lit_1] [[], []])
              [Bracketed "B"] [Bracketed "C"]
    , assertParseFail statement "print(); fails" "print();"
    ]
  , testGroup "reject"
    [ testStmt "reject(a,1);" $
        Reject [] (Printables [id_a, lit_1] [[], []]) [] []
    , testStmt "reject/*A*/(a,1)/*B*/;/*C*/" $
        Reject [Bracketed "A"] (Printables [id_a, lit_1] [[], []])
               [Bracketed "B"] [Bracketed "C"]
    , assertParseFail statement "reject(); fails" "reject();"
    ]
  , testGroup "empty stmt"
    [ testStmt ";" $ Empty []
    , testStmt ";/*A*/" $ Empty [Bracketed "A"]
    ]
  , testGroup "special cases"
    [ testStmt "returnn = a;" $ Assign (Identifier "returnn" []) [] id_a []
    , testStmt "break_ = a;" $ Assign (Identifier "break_" []) [] id_a []
    , testStmt "continue_ = a;" $ Assign (Identifier "continue_" []) [] id_a []
    , testStmt "if_ = a;" $ Assign (Identifier "if_" []) [] id_a []
    , testStmt "else_ = a;" $ Assign (Identifier "else_" []) [] id_a []
    ]
  , testGroup "top var types"
    [ testParser (varType True) "int" $ Int [] Nothing
    , testParser (varType True) "real" $ Real [] Nothing
    , testParser (varType False) "real'" $ Real [] Nothing
    , testParser (varType True) "complex" $ Complex [] Nothing
    , testParser (varType True) "int/*A*/" $ Int [Bracketed "A"] Nothing
    , testParser (varType True) "real/*A*/" $ Real [Bracketed "A"] Nothing
    , testParser (varType True) "complex/*A*/" $ Complex [Bracketed "A"] Nothing
    , testParser (varType True) "int<upper=a,lower=b>" $
      Int [] (Just $ VarConstraints
                       [Upper [] [] [] id_a, Lower [] [] [] id_b] [])
    , testParser (varType True) "int<lower=a,upper=b>" $
        Int [] (Just $ VarConstraints
                         [Lower [] [] [] id_a, Upper [] [] [] id_b] [])
    , testParser (varType True) "real<offset=b>" $
        Real [] (Just $ VarConstraints [Offset [] [] [] id_b] [])
    , testParserMsg (varType True) "annotated real"
      "real/*A*/</*B*/lower/*C*/=/*D*/a/*E*/,/*F*/upper/*G*/=/*H*/b/*I*/>/*J*/"
      $ Real [Bracketed "A"]
             (Just $ VarConstraints
                       [ Lower [Bracketed "B"] [Bracketed "C"]
                               [Bracketed "D"] (Identifier "a" [Bracketed "E"])
                       , Upper [Bracketed "F"] [Bracketed "G"]
                               [Bracketed "H"] (Identifier "b" [Bracketed "I"])
                       ] [Bracketed "J"])
    ]
  , testGroup "top var declarations"
    [ testParser (varDeclaration True True) "int x = a;" $
        VarDeclAssign (Int [] Nothing) id_x Nothing [] id_a []
    , testParser (varDeclaration True True) "real x = 1;" $
        VarDeclAssign (Real [] Nothing) id_x Nothing [] lit_1 []
    , testParser (varDeclaration True True) "real x;" $
        VarDecl (Real [] Nothing) id_x Nothing []
    , testParser (varDeclaration True True) "complex x[2, 3];" $
        VarDecl (Complex [] Nothing) id_x
                (Just $ ArrayDims [([], lit_2), ([], lit_3)] []) []
    , testParserMsg (varDeclaration True True) "annotated vec"
        "vector/*A*/[/*B*/3]/*C*/ x =/*D*/2;/*E*/" $
        VarDeclAssign (Vector [Bracketed "A"] Nothing [Bracketed "B"] lit_3
                        [Bracketed "C"])
                      id_x Nothing [Bracketed "D"] lit_2 [Bracketed "E"]
    , testParserMsg (varDeclaration True True) "annotated mat"
        "matrix/*A*/[/*B*/3,/*C*/1]/*D*/ x =/*E*/2;/*F*/" $
        VarDeclAssign (Matrix [Bracketed "A"] Nothing [Bracketed "B"] lit_3
                              [Bracketed "C"] lit_1 [Bracketed "D"])
                    id_x Nothing [Bracketed "E"] lit_2 [Bracketed "F"]
    , testParserMsg (varDeclaration True True) "cholesky_factor_mat"
        "cholesky_factor_cov[p] x;" $
        VarDecl (CholeskyFactorCov [] [] id_p Nothing []) id_x Nothing []
    , testParserMsg (varDeclaration True True) "cholesky_factor_mat'"
        "cholesky_factor_cov[p,q] x;" $
        VarDecl (CholeskyFactorCov [] [] id_p (Just $ ([], id_q)) []) id_x
                Nothing []
    ]
  , testGroup "top var declarations fail"
    [ assertParseFail (varDeclaration False True) "i)" "int<lower=0> i;"
    , assertParseFail (varDeclaration True False) "i)" "int<lower=0> i = 0;"
    , assertParseFail (varDeclaration True True) "i)" "real<> i;"
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
