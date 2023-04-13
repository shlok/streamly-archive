module Main where

import qualified Streamly.External.Archive.Tests
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "Streamly.External.Archive.Tests"
        Streamly.External.Archive.Tests.tests
    ]
