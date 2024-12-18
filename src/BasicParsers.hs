{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module BasicParsers
  ( Query(..),
    SubmarineType(..),
    OpenState(..),
    parseTask,
    parseTaskList,
    skipSpaces,
    parseLiteral,
    parseChar,
    parseString,
    parseInt,
    char,
    Parser,
    parse
  )
where


import Control.Applicative (Alternative (empty), (<|>), optional)
import Data.Char (isDigit)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)

data Query
  = CreateSubmarine SubmarineType String Int
  | ManipulateAirlock SubmarineType String OpenState
  | DestroySubmarine SubmarineType String Int 
  | TheSea SubmarineType
  | View 
  | Sequence [Query]
  deriving (Eq, Show)

data SubmarineType = Attack | Scout | DeepDive
 deriving (Eq, Show)

data OpenState = Open | Closed 
  deriving (Eq, Show)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)


parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (char ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

parseTask :: Parser Query
parseTask =
  parseCreateSubmarine <|>
  parseDestroySubmarine <|>
  parseManipulateAirlock <|>
  parseView <|>
  parseTheSea

parseCreateSubmarine :: Parser Query
parseCreateSubmarine = do
  _ <- parseLiteral "create_submarine"
  _ <- parseChar '('
  subType <- parseSubmarineType
  _ <- parseChar ','
  subName <- parseString
  _ <- parseChar ','
  balCount <- parseInt
  _ <- parseChar ')'
  return $ CreateSubmarine subType subName balCount

parseDestroySubmarine :: Parser Query
parseDestroySubmarine = do
  _ <- parseLiteral "destroy_submarine"
  _ <- parseChar '('
  subType <- parseSubmarineType
  _ <- parseChar ','
  subName <- parseString
  _ <- parseChar ','
  balCount <- parseInt
  _ <- parseChar ')'
  return $ DestroySubmarine subType subName balCount

-- <view> ::= "view" "(" ")"
parseView :: Parser Query
parseView = do
  _ <- parseLiteral "view"
  _ <- parseChar '('
  _ <- parseChar ')'
  return View

parseManipulateAirlock :: Parser Query
parseManipulateAirlock = do
    _ <- parseLiteral "manipulate_airlock"
    _ <- parseChar '('
    subType <- parseSubmarineType
    _ <- parseChar ','
    subName <- parseString
    _ <- parseChar ','
    openState <- parseOpenState
    _ <- parseChar ')'
    return $ ManipulateAirlock subType subName openState

parseSubmarineType :: Parser SubmarineType
parseSubmarineType =
    (parseLiteral "Attack" >> return Attack) <|>
    (parseLiteral "Scout" >> return Scout) <|>
    (parseLiteral "DeepDive" >> return DeepDive) 

parseOpenState :: Parser OpenState
parseOpenState =
    (parseLiteral "Open" >> return Open) <|>
    (parseLiteral "Closed" >> return Closed) 

parseTheSea :: Parser Query
parseTheSea = do
  _ <- parseLiteral "theSea"
  _ <- parseChar '('
  subType <- parseSubmarineType
  _ <- parseChar ')'
  return $ TheSea subType

sat :: (Char -> Bool) -> Parser Char
sat p = do
  input <- lift get
  case input of
    [] -> throwError "Empty String"
    (x:xs) -> if p x
              then lift (put xs) >> return x
              else throwError $ "Could not recognize: " ++ [x]

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char
skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = lift (modify (dropWhile (== ' ')))

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseString :: Parser String
parseString = do
  input <- lift get
  let input' = skipSpaces input
  if null input'
    then return ""
    else if head input' == '"'
         then parseQuotedString (tail input')
         else let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
              in lift (put rest) >> return str
  where
    parseQuotedString [] = throwError "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseInt :: Parser Int
parseInt = do
  input <- lift get
  let (digits, rest) = span isDigit (skipSpaces input)
  if null digits
    then throwError "Expected an integer"
    else do
      lift (put rest)
      return (read digits)