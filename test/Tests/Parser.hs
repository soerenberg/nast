module Tests.Parser (tests) where

import Text.Nast.Annotation (Annotation (..))
import Text.Nast.Expr (Expr (..))
import Text.Nast.Parser (literal, annotation)

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.Either (isLeft)


tests :: [TestTree]
tests =
  [ testGroup "literal"
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
  , testGroup "annotations"
    [ testCase "//" $ parse annotation "" "//" @?= (Right $ LineBased "")
    , testCase "// abc" $ parse annotation "" "// abc" @?=
        (Right $ LineBased " abc")
    , testCase "/**/" $ parse annotation "" "/**/" @?= (Right $ Bracketed "")
    , testCase "/* abc */" $ parse annotation "" "/* abc */" @?=
        (Right $ Bracketed " abc ")
    , testCase "/* ab*c*/" $ parse annotation "" "/* ab*c*/" @?=
        (Right $ Bracketed " ab*c")
    , testCase "\n" $ parse annotation "" "\n" @?= Right Newline
    ]
  ]
