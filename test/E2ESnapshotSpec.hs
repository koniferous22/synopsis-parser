module E2ESnapshotSpec
  ( snapshotTests
  ) where

import Test.HUnit

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isSpace)
import Data.Either (fromRight, isRight)
import SynopsisParser
import Text.Parsec

data SnapshotTest = SnapshotTest
  { snapshotTestLabel :: String
  , inputFile :: String
  , snapshotFile :: String
  }

trimSpace :: BS.ByteString -> BS.ByteString
trimSpace = BS.dropWhile isSpace . BS.dropWhileEnd isSpace

createSnapshotTest :: SnapshotTest -> Test
createSnapshotTest st =
  TestLabel (snapshotTestLabel st) . TestCase $ do
    input <- readFile . inputFile $ st
    let parserOutput =
          encode <$>
          parse synopsis ("Snapshot Test - " ++ snapshotTestLabel st) input
    assertBool "Parser result failed" (isRight parserOutput)
    let parsedJSON = fromRight BS.empty parserOutput
    snapshotContents <- trimSpace <$> (BS.readFile . snapshotFile $ st)
    assertEqual "Snapshot doesn't match" snapshotContents parsedJSON

snapshotTests :: Test
snapshotTests =
  TestList
    [ createSnapshotTest $
      SnapshotTest
        { snapshotTestLabel = "git"
        , inputFile = "test-resources/input/linux/man-git.txt"
        , snapshotFile = "test-resources/__snapshots/linux/git.json"
        }
    , createSnapshotTest $
      SnapshotTest
        { snapshotTestLabel = "git-init"
        , inputFile = "test-resources/input/linux/man-git-init.txt"
        , snapshotFile = "test-resources/__snapshots/linux/git-init.json"
        }
    , createSnapshotTest $
      SnapshotTest
        { snapshotTestLabel = "git-clone"
        , inputFile = "test-resources/input/linux/man-git-clone.txt"
        , snapshotFile = "test-resources/__snapshots/linux/git-clone.json"
        }
    ]
