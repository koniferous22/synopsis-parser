{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Parsec

import SynopsisParser

main :: IO ()
main = do
  input <- getContents
  case parse synopsis "Synopsis-parser" input of
    Left err -> print err
    Right result -> BS.putStrLn (encode result)
