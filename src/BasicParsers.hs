{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
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
    Parser (..)
  )
where

import Control.Applicative (Alternative (empty), (<|>), optional)
import Data.Char (isDigit)

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

newtype Parser a = P { parse :: String -> Either String (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

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
  _ <- parseLiteral "sell_vehicle"
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
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = P $ \input ->
  let input' = dropWhile (== ' ') input
   in Right ((), input')
parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseString :: Parser String
parseString = P $ \input ->
  let input' = skipSpaces input
   in if null input'
        then Right ("", "")
        else
          if head input' == '"'
            then parseQuotedString (tail input')
            else
              let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
               in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = Right ("", rest)
    parseQuotedString (x : rest) = case parseQuotedString rest of
      Right (str, rest') -> Right (x : str, rest')
      Left err -> Left err

parseInt :: Parser Int
parseInt = P $ \input ->
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)