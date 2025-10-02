module Specs.FooSpec where

import Test.Hspec

test :: SpecWith ()
test = do
  describe "stuff" $ do
    context "when things" $ do
      it "does stuf" $ do
        1 `shouldBe` 1
