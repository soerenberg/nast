module Tests.Parsers.Comments (tests) where

import Text.Nast.Comment (Comment (..))
import Text.Nast.Parsers.Comments (comment)

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: [TestTree]
tests =
  [ testGroup "lineBased"
    [ testCase "//" $ parse comment "" "//" @?= (Right $ LineBased "")
    , testCase "// abc" $ parse comment "" "// abc" @?=
        (Right $ LineBased " abc")
    , testCase "/**/" $ parse comment "" "/**/" @?= (Right $ Bracketed "")
    , testCase "/* abc */" $ parse comment "" "/* abc */" @?=
        (Right $ Bracketed " abc ")
    , testCase "/* ab*c*/" $ parse comment "" "/* ab*c*/" @?=
        (Right $ Bracketed " ab*c")
    ]
  ]
