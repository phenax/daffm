module Main (main) where

import qualified Specs.FooSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.FooSpec.test
