{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}

module Language.StructuredScript.ParsersSpec (main, spec) where

import ClassyPrelude
import Language.StructuredScript.Parsers
import Test.Hspec

main :: IO ()
main = hspec spec

-- ============Test case for evalExpr==============
testEvalExprInt = Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5));
testEvalExprDouble = Duo Add (Con (ConstDouble 5.2)) (Con (ConstDouble 5.6));
testEvalExprDoubleInt = Duo Add (Con (ConstDouble 5.2)) (Con (ConstInteger 5));
testEvalExprString = Duo Add (Con (ConstString "Test String")) (Con (ConstString "Testing String2"));
testEvalExprBool = Duo Add (Con (ConstBool True)) (Con (ConstBool False));
testEvalExprChar = Duo Add (Con (ConstChar 'T')) (Con (ConstChar 'F'));

testIsLeft (Left _ ) = True
testIsLeft ( _ ) = False


spec :: Spec
spec = do
  describe "evalExpr" $ do
    it "should return \"This is a test\"" $ do
                            (evalExpr emptyVTable testEvalExprInt) `shouldBe` (Right $ ConstInteger 10) 
                            (evalExpr emptyVTable testEvalExprDouble) `shouldBe` (Right $ ConstDouble 10.8)
                            (evalExpr emptyVTable testEvalExprDoubleInt) `shouldBe` (Right $ ConstDouble 10.2)
                           -- (testIsLeft.evalExpr emptyVTable $ testEvalExprString) `shouldBe` True
                           -- (testIsLeft.evalExpr $ emptyVTable testEvalExprBool) `shouldBe` True
                           -- (testIsLeft.evalExpr $ emptyVTable testEvalExprChar) `shouldBe` True
