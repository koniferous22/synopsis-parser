{-# LANGUAGE DeriveGeneric #-}

module SynopsisParser.Term
  ( FlagTerm(..)
  , FixedTerm(..)
  , SubstitutionTerm(..)
  , SubstitutionKeyValueTerm(..)
  , ParametrizedFlagTerm(..)
  , OptionalParametrizedFlagTerm(..)
  , flagTerm
  , fixedTerm
  , substitutionTerm
  , substitutionKeyValueTerm
  , orOperatorLiteral
  , parametrizedFlagTerm
  , optionalParametrizedFlagTerm
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.String (Parser)

import SynopsisParser.Utils

allowedTermChar :: Parser Char
allowedTermChar = alphaNum <|> char '-' <|> char '_'

constantFragment :: Parser String
constantFragment = many1 allowedTermChar

optionalConstantFragment :: Parser String
optionalConstantFragment = optionalBrackets constantFragment

flagFragment :: Parser String
flagFragment = do
  dashes <- atLeastAtMost 1 2 . char $ '-'
  fragment <- many allowedTermChar
  return . (++) dashes $ fragment

substitutionFragment :: Parser String
substitutionFragment = substitutionBrackets constantFragment

substitutionKeyValueFragment :: Parser String
substitutionKeyValueFragment =
  keyValuePairString '=' substitutionFragment substitutionFragment

newtype FlagTerm =
  FlagTerm String
  deriving (Eq, Generic, Show)

newtype FixedTerm =
  FixedTerm String
  deriving (Eq, Generic, Show)

newtype SubstitutionTerm =
  SubstitutionTerm String
  deriving (Eq, Generic, Show)

newtype OrOperatorLiteral =
  OrOperator Char
  deriving (Eq, Generic, Show)

newtype SubstitutionKeyValueTerm =
  SubstitutionKeyValueTerm String
  deriving (Eq, Generic, Show)

data ParametrizedFlagTerm = ParametrizedFlagTerm
  { parametrizedFlag :: String
  , parametrizedFlagArgument :: String
  } deriving (Eq, Generic, Show)

data OptionalParametrizedFlagTerm = OptionalParametrizedFlagTerm
  { optionalParametrizedFlag :: String
  , optionalParametrizedFlagArgument :: String
  } deriving (Eq, Generic, Show)

flagTerm :: Parser FlagTerm
flagTerm = do
  initialFragment <- flagFragment
  rest <- (many . choice) [optionalConstantFragment, constantFragment]
  return . FlagTerm . concat . (:) initialFragment $ rest

fixedTerm :: Parser FixedTerm
fixedTerm =
  FixedTerm . concat <$>
  (many1 . choice) [optionalConstantFragment, constantFragment]

substitutionTerm :: Parser SubstitutionTerm
substitutionTerm = SubstitutionTerm <$> substitutionFragment

substitutionKeyValueTerm :: Parser SubstitutionKeyValueTerm
substitutionKeyValueTerm =
  SubstitutionKeyValueTerm <$> substitutionKeyValueFragment

orOperatorLiteral :: Parser OrOperatorLiteral
orOperatorLiteral = OrOperator <$> char '|'

parametrizedFlagTerm :: Parser ParametrizedFlagTerm
parametrizedFlagTerm =
  let parametrizedFlagArg = do
        _ <- char '='
        choice [try substitutionKeyValueFragment, substitutionFragment]
   in do initialFlagFragment <- flagFragment
         flagRest <-
           (many . choice) [try optionalConstantFragment, constantFragment]
         ParametrizedFlagTerm (concat . (:) initialFlagFragment $ flagRest) <$>
           parametrizedFlagArg

optionalParametrizedFlagTerm :: Parser OptionalParametrizedFlagTerm
optionalParametrizedFlagTerm =
  let optionalParametrizedFlagArg =
        optionalBrackets
          (do _ <- char '='
              choice [try substitutionKeyValueFragment, substitutionFragment])
   in do initialFlagFragment <- flagFragment
         flagRest <-
           (many . choice) [try optionalConstantFragment, constantFragment]
         OptionalParametrizedFlagTerm
           (concat . (:) initialFlagFragment $ flagRest) <$>
           optionalParametrizedFlagArg

instance ToJSON FlagTerm

instance ToJSON FixedTerm

instance ToJSON SubstitutionTerm

instance ToJSON SubstitutionKeyValueTerm

instance ToJSON ParametrizedFlagTerm

instance ToJSON OptionalParametrizedFlagTerm

instance FromJSON FlagTerm

instance FromJSON FixedTerm

instance FromJSON SubstitutionTerm

instance FromJSON SubstitutionKeyValueTerm

instance FromJSON ParametrizedFlagTerm

instance FromJSON OptionalParametrizedFlagTerm
