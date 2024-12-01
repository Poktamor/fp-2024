{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Concurrent (forkIO, newChan)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO)
import Data.Char (isAlpha)
import Data.List
import Data.Ord
import Lib2 qualified
import Lib3 qualified
import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf1, suchThat)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QuickChecker

main :: IO ()
main = defaultMain tests
tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = 
  testGroup
  "Lib2 tests"
  [ testCase "CreateSubmarine command parsing" $
      Lib2.parseQuery "create_submarine(Attack, \"Aurora\", 3)"
        @?= Right (Lib2.Sequence [Lib2.CreateSubmarine Lib2.Attack "Aurora" 3]),
    testCase "CreateSubmarine command parsing2" $
      Lib2.parseQuery "create_submarine(DeepDive, \"BigRig\", 5)"
        @?= Right (Lib2.Sequence [Lib2.CreateSubmarine Lib2.DeepDive "BigRig" 5]),
    testCase "View command parsing" $
      Lib2.parseQuery "view()"
        @?= Right (Lib2.Sequence [Lib2.View]),
    testCase "theSea command parsing" $
        Lib2.parseQuery "theSea(Attack)"
          @?= Right (Lib2.Sequence [Lib2.TheSea Lib2.Attack]),
    testCase "Unrecognizable command parsing" $
        Lib2.parseQuery "Random unknown command"
          @?= Left "Unrecognized command",
    testCase "State transition with CreateSubmarine" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "create_submarine(DeepDive, \"BigRig\", 8)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "\nAdded DeepDive BigRig, Ballast count: 8"
                Lib2.submarines newState @?= [(Lib2.DeepDive, "BigRig", 8)]
              Left err -> error err
          Left err -> error err,
    testCase "State transition with View" $ do
        let initialState = Lib2.emptyState
        case Lib2.stateTransition initialState Lib2.View of
          Right (Just stateView, _) -> stateView @?= "Submarines:\nTheSea:\n"
          Left err -> error err,
    testCase "State transition with DestroySubmarine" $ do
        let initialState = Lib2.emptyState {Lib2.submarines = [(Lib2.Attack, "Aurora", 5)]}
        case Lib2.parseQuery "destroy_submarine(Attack, \"Aurora\", 5)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "\nDestroyed Attack Aurora"
                Lib2.submarines newState @?= []
              Left err -> error err
          Left err -> error err,
    testCase "State transition with destroy_submarine, when submarine does not exist" $ do
      let initialState = Lib2.emptyState
      case Lib2.parseQuery "destroy_submarine(Attack, \"Aurora\", 5)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Submarine not found in theSea"
            Right _ -> error "Expected error for non-existent vehicle"
        Left err -> error err
  ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof [Lib3.Single <$> arbitrary, Lib3.Batch <$> arbitrary]

instance Arbitrary Lib2.Query where
  arbitrary =
    oneof
      [ Lib2.CreateSubmarine <$> arbitrary <*> nonEmptyAlphabeticString <*> positiveInt,
        Lib2.ManipulateAirlock <$> arbitrary <*> arbitrary <*> arbitrary,
        Lib2.DestroySubmarine <$> arbitrary <*> nonEmptyAlphabeticString <*> positiveInt,
        Lib2.TheSea <$> arbitrary,
        pure Lib2.View
      ]

nonEmptyAlphabeticString :: Gen String
nonEmptyAlphabeticString = listOf1 (arbitrary `suchThat` isAlpha)

positiveInt :: Gen Int
positiveInt = arbitrary `suchThat` (> 0)

positiveDouble :: Gen Double
positiveDouble = do
  wholePart <- positiveInt
  fractionalPart <- positiveInt
  return (read (show wholePart ++ "." ++ show fractionalPart) :: Double)

instance Arbitrary Lib2.SubmarineType where
  arbitrary = elements [Lib2.Attack, Lib2.Scout, Lib2.DeepDive]

instance Arbitrary Lib2.OpenState where
  arbitrary = elements [Lib2.Open, Lib2.Closed]


propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ QuickChecker.testProperty "parseStatements . renderStatements == Right (statements, \"\")" $
        \statements ->
          Lib3.parseStatements (Lib3.renderStatements statements) == Right (statements, "")
    ]