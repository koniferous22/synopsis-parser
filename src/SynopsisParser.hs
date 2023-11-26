{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SynopsisParser
  ( FlagSectionTerm(..)
  , FlagSectionSequence(..)
  , FlagSection(..)
  , SynopsisTerm(..)
  , Synopsis(Synopsis)
  , flagSectionTerm
  , flagSection
  , synopsis
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Text.Parsec
import Text.Parsec.String (Parser)

import SynopsisParser.Term
import SynopsisParser.Utils (wrapOptionalBrackets)

data FlagSectionTerm
  = FlagSectionFlagTerm
      { flagSectionFlagTerm :: FlagTerm
      }
  | FlagSectionFixedTerm
      { flagSectionFixedTerm :: FixedTerm
      }
  | FlagSectionSubstitutionTerm
      { flagSectionSubstitutionTerm :: SubstitutionTerm
      }
  | FlagSectionSubstitutionKeyValueTerm
      { flagSectionSubstitutionKeyValueTerm :: SubstitutionKeyValueTerm
      }
  | FlagSectionParametrizedFlagTerm
      { flagSectionParametrizedFlagTerm :: ParametrizedFlagTerm
      }
  | FlagSectionOptionalParametrizedFlagTerm
      { flagSectionOptionalParametrizedFlagTerm :: OptionalParametrizedFlagTerm
      }
  deriving (Eq, Generic, Show)

flagSectionTerm :: Parser FlagSectionTerm
flagSectionTerm =
  choice
    [ try
        (FlagSectionOptionalParametrizedFlagTerm <$>
         optionalParametrizedFlagTerm)
    , try (FlagSectionParametrizedFlagTerm <$> parametrizedFlagTerm)
    , try (FlagSectionSubstitutionKeyValueTerm <$> substitutionKeyValueTerm)
    , try (FlagSectionSubstitutionTerm <$> substitutionTerm)
    , try (FlagSectionFlagTerm <$> flagTerm)
    , FlagSectionFixedTerm <$> fixedTerm
    ]

newtype FlagSectionSequence = FlagSectionSequence
  { flagSectionSequenceTerms :: [FlagSectionTerm]
  } deriving (Eq, Generic, Show)

newtype FlagSection = FlagSection
  { flagSectionSequences :: [FlagSectionSequence]
  } deriving (Eq, Generic, Show)

flagSection :: Parser FlagSection
flagSection =
  let flagSectionSequence =
        FlagSectionSequence <$> (spaces *> sepEndBy1 flagSectionTerm spaces)
      separator = orOperatorLiteral
   in FlagSection <$>
      wrapOptionalBrackets (sepBy1 flagSectionSequence separator)

data SynopsisTerm
  = SynopsisFixedTerm
      { synopsisFixedTerm :: FixedTerm
      }
  | SynopsisFlagSection
      { synopsisFlagSection :: FlagSection
      }
  | SynopsisSubstitutionTerm
      { synopsisSubstitutionTerm :: SubstitutionTerm
      }
  deriving (Eq, Generic, Show)

newtype Synopsis = Synopsis
  { synopsisTerms :: [SynopsisTerm]
  } deriving (Eq, Generic, Show)

synopsis :: Parser Synopsis
synopsis =
  let synopsisTerm =
        try (SynopsisFlagSection <$> flagSection) <|>
        try (SynopsisFixedTerm <$> fixedTerm) <|>
        SynopsisSubstitutionTerm <$> substitutionTerm
   in do spaces
         Synopsis <$> sepEndBy1 synopsisTerm spaces

instance ToJSON FlagSectionTerm

instance ToJSON FlagSectionSequence

instance ToJSON FlagSection

instance ToJSON SynopsisTerm

instance ToJSON Synopsis

instance FromJSON FlagSectionTerm

instance FromJSON FlagSectionSequence

instance FromJSON FlagSection

instance FromJSON SynopsisTerm

instance FromJSON Synopsis
