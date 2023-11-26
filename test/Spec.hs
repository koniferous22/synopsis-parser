{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import E2ESnapshotSpec (snapshotTests)
import SynopsisParserSpec (unitTests)
import Test.HUnit (runTestTT)

main :: IO ()
main = do
  putStrLn "Running Unit tests"
  _ <- runTestTT unitTests
  putStrLn "Running Snapshot tests"
  _ <- runTestTT snapshotTests
  return ()
