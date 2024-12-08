{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Concurrent (forkIO, newChan)
import Control.Concurrent.STM ( atomically, newTVarIO, readTVarIO)
import Data.Char (isAlpha)
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
    [ testCase "create submarine command parsing" $
        Lib2.parseQuery "create_submarine(Attack, \"Aurora\", 5)"
          @?= Right (Lib2.CreateSubmarine Lib2.Attack "Aurora" 5),
      testCase "PerformMaintenance command parsing" $
        Lib2.parseQuery "manipulate_airlock(Attack, \"Aurora\", Open)"
          @?= Right (Lib2.ManipulateAirlock Lib2.Attack "Aurora" Lib2.Open),
      testCase "Destroy submarine command parsing" $
        Lib2.parseQuery "destroy_submarine(DeepDive, \"BigRig\", 10)"
          @?= Right (Lib2.DestroySubmarine Lib2.DeepDive "BigRig" 10),
      testCase "theSea command parsing" $
        Lib2.parseQuery "theSea(Attack)"
          @?= Right (Lib2.TheSea Lib2.Attack),
      testCase "View command parsing" $
        Lib2.parseQuery "view()"
          @?= Right Lib2.View,
      testCase "Invalid command parsing" $
        Lib2.parseQuery "invalid command"
          @?= Left "Could not recognize: invalid command",
      testCase "State transition with CreateSubmarine" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "create_submarine(Attack, \"Aurora\", 5)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "Created Attack Aurora, Ballast count: 5"
                Lib2.submarines newState @?= [(Lib2.Attack, "Aurora", 5)]
              Left err -> error err
          Left err -> error err,
      testCase "State transition with View" $ do
        let initialState = Lib2.emptyState
        case Lib2.stateTransition initialState Lib2.View of
          Right (Just stateView, _) -> stateView @?= "Submarines:\nTheSea:\n"
          Left err -> error err,
      testCase "State transition with DestroySubmarine when submarine exists" $ do
        let initialState = Lib2.emptyState {Lib2.submarines = [(Lib2.Attack, "Aurora", 5)]}
        case Lib2.parseQuery "destroy_submarine(Attack, \"Aurora\", 5)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "Destroyed Attack Aurora"
                Lib2.submarines newState @?= []
              Left err -> error err
          Left err -> error err,
      testCase "State transition with DestroySubmarine when submarine does not exist" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "destroy_submarine(Attack, \"Aurora\", 5)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Left err -> err @?= "Submarine not found in theSea"
              Right _ -> error "Expected error for non-existent submarine"
          Left err -> error err,
      testCase "State transition with Sequence" $ do
        let initialState = Lib2.emptyState
            queries = [Lib2.CreateSubmarine Lib2.Attack "Aurora" 5, Lib2.CreateSubmarine Lib2.DeepDive "BigRig" 10]
        case Lib2.stateTransition initialState (Lib2.Sequence queries) of
          Right (Just msg, newState) -> do
            msg @?= "\nCreated Attack Aurora, Ballast count: 5\nCreated DeepDive BigRig, Ballast count: 10"
            Lib2.submarines newState @?= [(Lib2.Attack, "Aurora", 5), (Lib2.DeepDive, "BigRig", 10)]
          Left err -> error err,
      testCase "SaveCommand state transition" $ do
        initialState <- newTVarIO Lib2.emptyState
        ioChan <- newChan
        _ <- forkIO $ Lib3.storageOpLoop ioChan
        result <- Lib3.stateTransition initialState Lib3.SaveCommand ioChan
        result @?= Right (Just "State saved successfully"),
      testCase "Batch processing with valid queries" $ do
        initialState <- newTVarIO Lib2.emptyState
        let queries = Lib3.Batch [Lib2.CreateSubmarine Lib2.Attack "Aurora" 5, Lib2.CreateSubmarine Lib2.DeepDive "BigRig" 10]
        result <- atomically $ Lib3.atomicStatements initialState queries
        case result of
          Right msg -> do
            msg @?= Just "Created Attack Aurora, Ballast count: 5\nCreated DeepDive BigRig, Ballast count: 10"
            newState <- readTVarIO initialState
            Lib2.submarines newState @?= [(Lib2.DeepDive, "BigRig", 10), (Lib2.Attack, "Aurora", 5)]
          Left err -> error err,
      testCase "Batch processing with invalid query" $ do
        initialState <- newTVarIO Lib2.emptyState
        let queries = Lib3.Batch [Lib2.CreateSubmarine Lib2.Attack "Aurora" 5, Lib2.DestroySubmarine Lib2.DeepDive "BigRig" 10]
        result <- atomically $ Lib3.atomicStatements initialState queries
        result @?= Left "Submarine not found in theSea"
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
