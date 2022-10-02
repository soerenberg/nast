module Tests.Expr (tests) where

import Text.Nast.Expr (Expr (..), annotate)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


litOne :: Expr a
litOne = NumLiteral "1" Nothing Nothing

tests :: [TestTree]
tests = [ testGroup "annotate"
            [ testCase "empty list" $
              annotate (litOne :: Expr ()) [] @?= litOne
            , testCase "list 1 elmnt" $
              annotate litOne ["outer"] @?= (Annotate "outer" litOne)
            , testCase "list 1 elmnt'" $
              annotate (Annotate "inner" $ litOne) ["outer"] @?=
              (Annotate "outer" (Annotate "inner" litOne))
            , testCase "list 2 elmnt" $
                annotate (Annotate "inner" $ litOne) ["outer", "mid"] @?=
                (Annotate "outer" $ Annotate "mid" $ Annotate "inner" litOne)
            ]
        ]
