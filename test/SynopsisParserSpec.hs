module SynopsisParserSpec
  ( unitTests
  ) where

import Test.HUnit

import SynopsisParser
import SynopsisParser.Term
import Text.Parsec

data UnitTest = UnitTest
  { testLabel :: String
  , input :: String
  , expectedResult :: Either ParseError Synopsis
  }

createUnitTest :: UnitTest -> Test
createUnitTest t =
  let actualResult = parse synopsis (testLabel t) (input t)
   in TestLabel (testLabel t) .
      TestCase . assertEqual (testLabel t) (expectedResult t) $
      actualResult

testSynopsis1 :: Test
testSynopsis1 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis1"
    , input = "[--[no-]remote-submodules]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence
                [FlagSectionFlagTerm . FlagTerm $ "--[no-]remote-submodules"]
            ]
          ]
    }

testSynopsis2 :: Test
testSynopsis2 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis2"
    , input = "[--]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [FlagSectionSequence [FlagSectionFlagTerm . FlagTerm $ "--"]]
          ]
    }

testSynopsis3 :: Test
testSynopsis3 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis3"
    , input = "[--git-dir=<path>]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence
                [ FlagSectionParametrizedFlagTerm .
                  ParametrizedFlagTerm "--git-dir" $
                  "<path>"
                ]
            ]
          ]
    }

testSynopsis4 :: Test
testSynopsis4 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis4"
    , input = "[-C <path>]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence
                [ FlagSectionFlagTerm (FlagTerm "-C")
                , FlagSectionSubstitutionTerm (SubstitutionTerm "<path>")
                ]
            ]
          ]
    }

testSynopsis5 :: Test
testSynopsis5 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis5"
    , input = "[--config-env=<name>=<envvar>]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence
                [ FlagSectionParametrizedFlagTerm
                    (ParametrizedFlagTerm "--config-env" "<name>=<envvar>")
                ]
            ]
          ]
    }

testSynopsis6 :: Test
testSynopsis6 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis6"
    , input = "[--recurse-submodules[=<pathspec>]]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence
                [ FlagSectionOptionalParametrizedFlagTerm
                    (OptionalParametrizedFlagTerm
                       "--recurse-submodules"
                       "[<pathspec>]")
                ]
            ]
          ]
    }

testSynopsis7 :: Test
testSynopsis7 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis7"
    , input = "[-c <name>=<value>]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence
                [ FlagSectionFlagTerm (FlagTerm "-c")
                , FlagSectionSubstitutionKeyValueTerm
                    (SubstitutionKeyValueTerm "<name>=<value>")
                ]
            ]
          ]
    }

testSynopsis8 :: Test
testSynopsis8 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis8"
    , input = "[-v | --version]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [ FlagSectionSequence [FlagSectionFlagTerm (FlagTerm "-v")]
            , FlagSectionSequence [FlagSectionFlagTerm (FlagTerm "--version")]
            ]
          ]
    }

testSynopsis9 :: Test
testSynopsis9 =
  createUnitTest $
  UnitTest
    { testLabel = "testSynopsis9"
    , input = "[-n]"
    , expectedResult =
        Right $
        Synopsis
          [ SynopsisFlagSection . FlagSection $
            [FlagSectionSequence [FlagSectionFlagTerm (FlagTerm "-n")]]
          ]
    }

unitTests :: Test
unitTests =
  TestList
    [ testSynopsis1
    , testSynopsis2
    , testSynopsis3
    , testSynopsis4
    , testSynopsis5
    , testSynopsis6
    , testSynopsis7
    , testSynopsis8
    , testSynopsis9
    ]
