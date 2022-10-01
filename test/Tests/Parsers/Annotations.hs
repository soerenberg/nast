module Tests.Parsers.Annotations (tests) where

import Text.Nast.Annotation (Annotation (..))
import Text.Nast.Parsers.Annotations (annotation)

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: [TestTree]
tests =
  [ testGroup "lineBased"
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
