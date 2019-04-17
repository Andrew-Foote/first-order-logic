module Main where

import Logic

descValidity :: Bool -> String
descValidity True = "Valid"
descValidity False = "Invalid"

main :: IO ()
main = do
    [fname] <- getArgs
    s <- readFile fname
    putStrLn $ descValidity $ proofValid $ parseProof s