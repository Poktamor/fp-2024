{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( Query(..),
    SubmarineType (..),
    OpenState (..),
    parseTask,
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import Data.Char 
import BasicParsers

data State = State
  { submarines :: [(SubmarineType, String, Int)],
    theSea :: [(SubmarineType, Int)] 
  }
  deriving (Eq, Show)

emptyState :: State
emptyState = State {submarines = [], theSea = []}


parseQuery :: String -> Either String Query
parseQuery s =
  case parse parseTaskList s of
    Left e -> Left e
    Right (qs, r) -> if null r
      then case qs of
        [q] -> Right q
        _ -> Right (Sequence qs)
      else Left ("Unrecognized characters: " ++ r)

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  CreateSubmarine subType subName balCount ->
    let updatedSubmarines = (subType, subName, balCount) : submarines st
        updatedSea = addToSea subType 1 (theSea st)
        newState = st {submarines = updatedSubmarines, theSea = updatedSea}
     in Right (Just ("Created " ++ show subType ++ " " ++ subName ++ ", "++ "Ballast count: " ++ show balCount ), newState)
  TheSea subType ->
    let submarineList = filter (\(t, _, _) -> t == subType) (submarines st)
        submarineStr = unlines $ map (\(_, n, b) -> n ++ ", Ballast count: " ++ show b) submarineList
     in Right (Just ("The sea now has " ++ show subType ++ ":\n" ++ submarineStr), st)
  View ->
    let submarineStr = unlines $ map (\(t, n, b) -> show t ++ " " ++ n ++ "Ballast count: " ++ show b) (submarines st)
        theSeaStr = unlines $ map (\(s, q) -> show q ++ " " ++ show s) (theSea st)
     in Right (Just ("Submarines:\n" ++ submarineStr ++ "TheSea:\n" ++ theSeaStr), st)
  DestroySubmarine subType subName ballastCount ->
    if any (\(t, n, b) -> t == subType && n == subName && b == ballastCount) (submarines st)
      then
        let newSubmarines = filter (\(t, n, b) -> not (t == subType && n == subName && b == ballastCount)) (submarines st)
            updatedTheSea = removeFromTheSea subType 1 (theSea st)
            newState = st {submarines = newSubmarines, theSea = updatedTheSea}
         in Right (Just ("Destroyed " ++ show subType ++ " " ++ subName ), newState)
      else Left "Submarine not found in theSea"
  ManipulateAirlock subType subName openState ->
    Right (Just ("The " ++ show subType ++ " submarine " ++ subName ++ "\'s airlock is now " ++ show openState ), st)
  Sequence queryList ->
    foldl processQuery (Right (Just "", st)) queryList
    where
      processQuery :: Either String (Maybe String, State) -> Query -> Either String (Maybe String, State)
      processQuery (Left err) _ = Left err
      processQuery (Right (accMsg, currentState)) nextQuery =
        case stateTransition currentState nextQuery of
          Left err -> Left err
          Right (Just result, newState) ->
            Right (combineMessages accMsg (Just result), newState)
          Right (Nothing, newState) -> Right (accMsg, newState)

addToSea :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
addToSea item quantity [] = [(item, quantity)]
addToSea item quantity ((i, q) : xs)
  | i == item = (i, q + quantity) : xs
  | otherwise = (i, q) : addToSea item quantity xs

removeFromTheSea :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
removeFromTheSea _ _ [] = []
removeFromTheSea item quantity ((i, q) : xs)
  | i == item = if q > quantity then (i, q - quantity) : xs else xs
  | otherwise = (i, q) : removeFromTheSea item quantity xs

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg) Nothing = Just msg
combineMessages Nothing (Just msg) = Just msg
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)
