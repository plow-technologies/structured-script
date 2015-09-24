{- |
Module      :  <Main>
Description :  <Main for StructuredScript>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable

<Main function for STructuredScript>
-}

{-# LANGUAGE OverloadedStrings #-}
import           Data.Text

main :: IO ()
main = print test
    where test::Text
          test = "test"

