module Main where

import           Test.Framework         (defaultMain)

import qualified Data.Bson.Binary.Tests
import qualified Data.Bson.Tests

main :: IO ()
main = defaultMain
    [ Data.Bson.Tests.tests
    , Data.Bson.Binary.Tests.tests
    ]
