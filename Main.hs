{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Data.Text (Text)

data Person = Person {
        age :: Int,
        name :: Text,
        pets :: [Pet]
    }

data Pet = Pet {
        petName :: Text,
        petAge :: Int
    }

main :: IO ()
main = putStrLn "Hello, Haskell!"
