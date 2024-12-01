module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [
        "manipulate_airlock",
        "create_submarine",
        "destroy_submarine",
        "theSea",
        "view",
        "Attack",
        "Scout",
        "DeepDive",
        "open",
        "closed"
    ]