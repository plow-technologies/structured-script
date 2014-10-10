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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           ClassyPrelude

main :: IO ()
main = print test
    where test::Text
          test = "test"

