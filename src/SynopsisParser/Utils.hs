module SynopsisParser.Utils
  ( brackets
  , optionalBrackets
  , substitutionBrackets
  , wrapOptionalBrackets
  , atLeastAtMost
  , keyValuePairString
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

brackets :: Char -> Char -> Parser String -> Parser String
brackets c1 c2 p = do
  l <- char c1
  result <- p
  r <- char c2
  return (l : (result ++ [r]))

optionalBrackets :: Parser String -> Parser String
optionalBrackets = brackets '[' ']'

substitutionBrackets :: Parser String -> Parser String
substitutionBrackets = brackets '<' '>'

wrapChars :: Char -> Char -> Parser a -> Parser a
wrapChars c1 c2 p = do
  _ <- char c1
  result <- p
  _ <- char c2
  return result

wrapOptionalBrackets :: Parser a -> Parser a
wrapOptionalBrackets = wrapChars '[' ']'

atLeastAtMost :: Int -> Int -> Parser a -> Parser [a]
atLeastAtMost least most p = do
  x <- count least p
  y <- option [] . count (most - least) $ p
  return (x ++ y)

keyValuePairString :: Char -> Parser String -> Parser String -> Parser String
keyValuePairString c pa pb = do
  k <- pa
  d <- char c
  (++) k . (d :) <$> pb
