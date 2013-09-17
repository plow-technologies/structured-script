{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}

module Language.StructuredScriptSpec (main, spec) where

import ClassyPrelude
import Language.StructuredScript
import Test.Hspec

main :: IO ()
main = hspec spec


testZBeginRslt :: Text 
testZBeginRslt = "test"

spec :: Spec
spec = do
  describe "testZBegin" $ do
    it "should return \"This is a test\"" $ do
                            rslt <-  testZBegin 
                            rslt `shouldBe` testZBeginRslt                 

