{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Lens hiding (view)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.String.Conversions (cs)
import Network.Wreq hiding (get)
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Environment (getArgs)

data Command next
  = CreateSubmarine String String Int next
  | ManipulateAirlock String next
  | DestroySubmarine String String Int next
  | TheSea String (String -> next)
  | View (String -> next )
  deriving Functor

type SubmarineDSL = Free Command

createSubmarine :: String -> String -> Int -> SubmarineDSL ()
createSubmarine subType subName ballastCount = liftF $ CreateSubmarine subType subName ballastCount ()

manipulateAirlock :: String -> SubmarineDSL ()
manipulateAirlock openState = liftF $ ManipulateAirlock openState ()

destroySubmarine :: String -> String -> Int -> SubmarineDSL ()
destroySubmarine subType subName ballastCount = liftF $ DestroySubmarine subType subName ballastCount ()

theSea :: String -> SubmarineDSL String
theSea subType = liftF $ TheSea subType id

view :: SubmarineDSL String
view = liftF $ View id


runHttpSingle :: SubmarineDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (CreateSubmarine subType subName ballastCount next)) = do
  putStrLn $ "Sending request: create_submarine(" ++ subType ++ ", " ++ subName ++ ", " ++ show ballastCount ++ ")"
  _ <- post "http://localhost:3000" (cs $ "create_submarine(" ++ subType ++ ", " ++ subName ++ ", " ++ show ballastCount ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (ManipulateAirlock openState next)) = do
  putStrLn $ "Sending request: manipulate_airlock(" ++ openState ++ ")"
  _ <- post "http://localhost:3000" (cs $ "manipulate_airlock(" ++ openState ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (DestroySubmarine subType subName ballastCount next)) = do
  putStrLn $ "Sending request: destroy_submarine(" ++ subType ++ ", " ++ subName ++ ", " ++ show ballastCount ++ ")"
  _ <- post "http://localhost:3000" (cs $ "destroy_submarine(" ++ subType ++ ", " ++ subName ++ ", " ++ show ballastCount ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (TheSea subType next)) = do
  putStrLn $ "Sending request: theSea(" ++ subType ++ ")"
  resp <- post "http://localhost:3000" (cs $ "theSea(" ++ subType ++ ")" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)
runHttpSingle (Free (View next)) = do
  putStrLn "Sending request: view()"
  resp <- post "http://localhost:3000" (cs "view()" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpBatch :: SubmarineDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> SubmarineDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  return a
runHttpBatch' acc (Free (CreateSubmarine subType subName ballastCount next)) =
  runHttpBatch' (acc ++ ["create_submarine(" ++ subType ++ ", " ++ subName ++ ", " ++ show ballastCount ++ ")"]) next
runHttpBatch' acc (Free (ManipulateAirlock openState next)) =
  runHttpBatch' (acc ++ ["manipulate_airlock(" ++ openState ++ ")"]) next
runHttpBatch' acc (Free (DestroySubmarine subType subName ballastCount next)) =
  runHttpBatch' (acc ++ ["destroy_submarine(" ++ subType ++ ", " ++ subName ++ ", " ++ show ballastCount ++ ")"]) next
runHttpBatch' acc (Free (TheSea subType next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs $ "theSea(" ++ subType ++ ")" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)
runHttpBatch' acc (Free (View next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs "view()" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

type InMemoryState = [(String, String, Int)]

runInMemory :: SubmarineDSL a -> State InMemoryState a
runInMemory (Pure a) = return a
runInMemory (Free (CreateSubmarine subType subName ballastCount next)) = do
  modify ((subType, subName, ballastCount) :)
  runInMemory next
runInMemory (Free (ManipulateAirlock _ next)) = runInMemory next
runInMemory (Free (DestroySubmarine subType subName ballastCount next)) = do
  modify (filter (\(t, n, b) -> not (t == subType && n == subName && b == ballastCount)))
  runInMemory next
runInMemory (Free (TheSea subType next)) = do
  currentState <- Control.Monad.State.get
  let filteredInventory = filter (\(t, _, _) -> t == subType) currentState
  runInMemory (next $ show filteredInventory)
runInMemory (Free (View next)) = do
  currentState <- Control.Monad.State.get
  runInMemory (next $ show currentState)

main :: IO ()
main = do
  args <- getArgs
  let program = do
        createSubmarine "Attack" "Aurora"  5
        manipulateAirlock "Open"
        _ <- theSea "Attack"
        _ <- destroySubmarine "Attack" "Aurora" 5
        view

  case args of
    ["single"] -> do
      putStrLn "Running with HTTP single request per command:"
      _ <- runHttpSingle program
      return ()
    ["batch"] -> do
      putStrLn "Running with HTTP batch requests:"
      _ <- runHttpBatch program
      return ()
    ["memory"] -> do
      putStrLn "Running with in-memory interpreter for testing:"
      let (result, finalState) = runState (runInMemory program) []
      print result
      print finalState
    _ -> putStrLn "Usage: stack exec fp2024-four-client [single|batch|memory]"