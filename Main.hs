{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Lenteja

data Person = Person
  { age :: Int,
    name :: Text,
    pets :: [Pet],
    partner :: Person
  }

data Pet = Pet
  { petName :: Text,
    petAge :: Int
  }

it :: Person
it =
  let john = Person 41 "John" [fido] sara
      sara = Person 43 "Sara" [] john
      fido = Pet "Fido" 4
   in john

main :: IO ()
main = putStrLn "Hello, Haskell!"
