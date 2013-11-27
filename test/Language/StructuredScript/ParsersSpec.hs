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

testString2 = "x:= input1; y:= input2; b1:= True; b2:= False; b3:= b1 && b2; set:= 0 isSet 4;i3:= x XOR y; s1:= \"This is a test program.\"; if (~(b1 XOR b2) && (x > 7)) then z:= x - (-x MOD y) ; s2:= \"The result is \" CONCAT z ; else z:= y;end_if/*; x:=y */; output:= z"


-- | General Test Case
-- ============Test case for sstTest==============
testCaseBase = "x:= input1; y:= input2;"
input1 = "output:= x"
input2 = "y"
listInteger = [18, 7]
testListInteger = ConstInteger <$> listInteger
testListDouble = ConstDouble <$> [7.7, 18.8]
testListString = ConstString <$> ["Test String1", "Test String2"]
testListBool = ConstBool <$> [True, False]
testListChar = ConstChar <$> ['T', 'F']

-- | Test Case for Addition
testSSTAdd = testCaseBase ++ input1 ++ " + " ++ input2
-- | Test Case for Subtraction
testSSTSub = testCaseBase ++ input1 ++ " - " ++ input2
-- | Test Case for Multiplication
testSSTMul = testCaseBase ++ input1 ++ " * " ++ input2
-- | Test Case for Division
testSSTDiv = testCaseBase ++ input1 ++ " / " ++ input2
-- | Test Case for Modular
testSSTMod = testCaseBase ++ input1 ++ " MOD " ++ input2
-- | Test Case for Exponent
testSSTPow = testCaseBase ++ input1 ++ " ** " ++ input2

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
      -- | Test sst for Addition
      it "should return \"Test for Addition\"" $ do
        (sstTest testListInteger testSSTAdd) `shouldBe` (Right $ ConstInteger $ (18 + 7)) 
        (sstTest testListDouble  testSSTAdd) `shouldBe` (Right $ ConstDouble  $ (7.7 + 18.8))
        (testIsLeft.sstTest testListString $ testSSTAdd) `shouldBe` True
        (testIsLeft.sstTest testListBool $ testSSTAdd) `shouldBe` True
        (testIsLeft.sstTest testListChar $ testSSTAdd) `shouldBe` True

      -- | Test sst for Subtraction
      it "should return \"Test for Subtraction\"" $ do
        (sstTest testListInteger testSSTSub) `shouldBe` (Right $ ConstInteger $ (18 - 7)) 
        (sstTest testListDouble  testSSTSub) `shouldBe` (Right $ ConstDouble  $ (7.7 - 18.8))
        (testIsLeft.sstTest testListString $ testSSTSub) `shouldBe` True
        (testIsLeft.sstTest testListBool $ testSSTSub) `shouldBe` True
        (testIsLeft.sstTest testListChar $ testSSTSub) `shouldBe` True

      -- | Test sst for Multiplication
      it "should return \"Test for Multiplication\"" $ do
        (sstTest testListInteger testSSTMul) `shouldBe` (Right $ ConstInteger $ (18 * 7)) 
        (sstTest testListDouble  testSSTMul) `shouldBe` (Right $ ConstDouble  $ (7.7 * 18.8))
        (testIsLeft.sstTest testListString $ testSSTMul) `shouldBe` True
        (testIsLeft.sstTest testListBool $ testSSTMul) `shouldBe` True
        (testIsLeft.sstTest testListChar $ testSSTMul) `shouldBe` True

      -- | Test sst for Division
      it "should return \"Test for Division\"" $ do
        (sstTest testListInteger testSSTDiv) `shouldBe` (Right $ ConstInteger $ (18 `div` 7)) 
        (sstTest testListDouble  testSSTDiv) `shouldBe` (Right $ ConstDouble  $ (7.7 / 18.8))
        (testIsLeft.sstTest testListString $ testSSTDiv) `shouldBe` True
        (testIsLeft.sstTest testListBool $ testSSTDiv) `shouldBe` True
        (testIsLeft.sstTest testListChar $ testSSTDiv) `shouldBe` True
      context "special case \"Divided zero\"" $ do 
	it "should return \"a Left Error\"" $ do
	(testIsLeft.sstTest testListInteger $ (testCaseBase ++ "y:= 0;" ++ input1 ++ " / " ++ input2)) `shouldBe` True

      -- | Test sst for Modular
      it "should return \"Test for Modular\"" $ do
        (sstTest testListInteger testSSTMod) `shouldBe` (Right $ ConstInteger $ (18 `mod` 7)) 
     	(testIsLeft.sstTest testListDouble $ testSSTMod) `shouldBe` True
        (testIsLeft.sstTest testListString $ testSSTMod) `shouldBe` True
        (testIsLeft.sstTest testListBool $ testSSTMod) `shouldBe` True
        (testIsLeft.sstTest testListChar $ testSSTMod) `shouldBe` True
      context "special case \"modular zero\"" $ do 
	it "should return \"a Left Error\"" $ do
	(testIsLeft.sstTest testListInteger $ (testCaseBase ++ "y:= 0;" ++ input1 ++ " / " ++ input2)) `shouldBe` True

      -- | Test sst for Overall Testing
      it "should return \"Test for Intergration\"" $ do
        (sstTest testListInteger testString2) `shouldBe` (Right $ ConstInteger $ (18 - (-18 `mod` 7))) 

      it "should return \"Test for Exponent\"" $ do
        (sstTest testListInteger testSSTPow) `shouldBe` (Right $ ConstInteger $ (18 ^ 7)) 
        (sstTest testListDouble (testCaseBase ++ "y:= 2;" ++ input1 ++ " ** " ++ input2)) `shouldBe` (Right $ ConstDouble $ (7.7 ^ 2)) 
      
                          
