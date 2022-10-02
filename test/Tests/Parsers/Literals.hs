module Tests.Parsers.Literals (tests) where

import Text.Nast.Parsers.Literals (numLiteral)
import Text.Nast.Expr (Expr (..))

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: [TestTree]
tests = [ testGroup "numLiteral"
          [ testCase "23" $ parse numLiteral "" "23" @?=
            (Right $ NumLiteral "23" Nothing Nothing)
          , testCase "-7" $ parse numLiteral "" "-7" @?=
            (Right $ NumLiteral "-7" Nothing Nothing)
          , testCase "0" $ parse numLiteral "" "0" @?=
            (Right $ NumLiteral "0" Nothing Nothing)
          , testCase "12.23" $ parse numLiteral "" "12.23" @?=
            (Right $ NumLiteral "12" (Just "23") Nothing)
          , testCase "03.14" $ parse numLiteral "" "03.14" @?=
            (Right $ NumLiteral "03" (Just "14") Nothing)
          , testCase "-13.1e07" $ parse numLiteral "" "-13.1e07" @?=
            (Right $ NumLiteral "-13" (Just "1") (Just "07"))
          , testCase "3e6" $ parse numLiteral "" "3e6" @?=
            (Right $ NumLiteral "3" Nothing (Just "6"))
          , testCase "-98e-012" $ parse numLiteral "" "-98e-012" @?=
            (Right $ NumLiteral "-98" Nothing (Just "-012"))
          , testCase "003.001e006" $ parse numLiteral "" "003.001e006" @?=
            (Right $ NumLiteral "003" (Just "001") (Just "006"))
          ]
        ]
