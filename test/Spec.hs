{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Hake
import Data.Aeson

main :: IO ()
main = hspec $ do
  describe "desugar" $ do
    it "desugars a single argument of valid JSON into JSON" $ do
      desugar ["{ \"a\" : \"b\" }"] `shouldBe` Just (object [("a", String "b")])
    it "desugars key-value pairs into a JSON object" $ do
      desugar ["a=b", "c=d"] `shouldBe` Just (object [("a", String "b"), ("c", String "d")])