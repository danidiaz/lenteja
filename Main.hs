{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Data.Generics.Product.Fields (field)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import GHC.Generics
import Lenteja
import Lenteja.Parser
import Type.Reflection
import Control.Exception
import Data.Foldable (traverse_)

data Person = Person
  { age :: Int,
    name :: Text,
    pets :: [Pet],
    partner :: Person
  }
  deriving (Show, Generic)

instance HasLentejas Person where
  lentejas =
    Map.fromList
      [ ("age", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"age")))),
        ("name", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"name")))),
        ("pets", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"pets")))),
        ("partner", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"partner"))))
      ]

data Pet = Pet
  { petName :: Text,
    petAge :: Int
  }
  deriving (Show, Generic)

instance HasLentejas Pet where
  lentejas =
    Map.fromList
      [ ("petName", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"petName")))),
        ("petAge", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"petAge"))))
      ]

it :: Person
it =
  let john = Person 41 "John" [fido,fifi] sara
      sara = Person 43 "Sara" [] john
      fido = Pet "Fido" 4
      fifi = Pet "Fifi" 3
   in john

repl :: IO ()
repl = do
  putStrLn "Enter lensy exp: "
  line <- getLine
  opticNames <- parseLensyExp (Data.Text.pack line)
  case inspect it opticNames of
    Left error -> throwIO error
    Right (SingleResult result) -> 
      do putStrLn "A single result:"
         putStrLn result
    Right (MultipleResults results) -> 
      do putStrLn "Multiple results:"
         traverse_ putStrLn results
  repl

main :: IO ()
main = do
  repl
