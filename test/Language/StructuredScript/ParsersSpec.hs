{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}

module Language.StructuredScript.ParsersSpec (main, spec) where

import ClassyPrelude
import Language.StructuredScript.Parsers
import Test.Hspec

main :: IO ()
main = hspec spec

-- ============Test case for evalExpr==============
-- | Test Case for Addition
testEvalExprInt = Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5));
testEvalExprDouble = Duo Add (Con (ConstDouble 5.2)) (Con (ConstDouble 5.6));
testEvalExprDoubleInt = Duo Add (Con (ConstDouble 5.2)) (Con (ConstInteger 5));
testEvalExprString = Duo Add (Con (ConstString "Test String1")) (Con (ConstString "Testing String2"));
testEvalExprBool = Duo Add (Con (ConstBool True)) (Con (ConstBool False));
testEvalExprChar = Duo Add (Con (ConstChar 'T')) (Con (ConstChar 'F'));

testIsLeft (Left _ ) = True
testIsLeft ( _ ) = False

testString2 = "x:= input1; y:= input2; b1:= True; b2:= False; set:= 0 isSet 4;/*c1:= \"c\"; c2:= \"2\"; c3:= c1 AND c2;*/ i3:= x XOR y; s1:= \"This is a test program.\"; if (~(b1 XOR b2) && (x > 7)) then z:= x - (-x MOD y) ; s2:= \"The result is \" CONCAT z ; else z:= y;end_if/*; x:=y */; output:= z"


-- ============Test case for sstTest==============
-- | Test Case for Subtraction
testCase = "x:= input1; y:= input2; output:= x - y"
testListInteger = ConstInteger <$> [18, 7]
testListDouble = ConstDouble <$> [7.7, 18.8]
testListString = ConstString <$> ["Test String1", "Test String2"]
testListBool = ConstBool <$> [True, False]
testListChar = ConstChar <$> ['T', 'F']


spec :: Spec
spec = do
  describe "evalExpr" $ do
    it "should return \"Test for evalExpr\"" $ do
        (evalExpr emptyVTable testEvalExprInt) `shouldBe` (Right $ ConstInteger 10)
        (evalExpr emptyVTable testEvalExprDouble) `shouldBe` (Right $ ConstDouble 10.8)
        (evalExpr emptyVTable testEvalExprDoubleInt) `shouldBe` (Right $ ConstDouble 10.2)
        (testIsLeft.evalExpr emptyVTable $ testEvalExprString) `shouldBe` True
        (testIsLeft.evalExpr emptyVTable $ testEvalExprBool) `shouldBe` True
        (testIsLeft.evalExpr emptyVTable $ testEvalExprChar) `shouldBe` True

  describe "Intergation Test for sstTest" $ do
      it "should return \"Test for Subtraction\"" $ do
    -- | Test evalExpr for Subtraction
        (sstTest testListInteger testCase) `shouldBe` (Right $ ConstInteger 11) 
        (sstTest testListDouble  testCase) `shouldBe` (Right $ ConstDouble  (7.7-18.8))
        (testIsLeft.sstTest testListString $ testCase) `shouldBe` True
        (testIsLeft.sstTest testListBool $ testCase) `shouldBe` True
        (testIsLeft.sstTest testListChar$ testCase) `shouldBe` True
                          
