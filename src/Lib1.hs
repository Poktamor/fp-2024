module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [
        "fill_ballasts",
        "empty_ballasts",
        "manipulate_airlock",
        "ballast_count",
        "open",
        "closed"

    ]
