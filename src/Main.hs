{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
import ClassyPrelude
import Language.StructuredScript 



main :: IO () 
main = do
  print test 
    where test::Text 
          test = "test"

cowabunga :: IO () 
cowabunga = do 
  print test 
    where test::Text 
          test = "test"
