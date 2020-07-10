--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Streamly.External.Archive.Tests

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
        [ testGroup "Streamly.External.Archive.Tests"
                     Streamly.External.Archive.Tests.tests ]

--------------------------------------------------------------------------------
