{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics
import Control.Lens 
import Type.Reflection
import Data.Generics.Product.Fields (field)
import Lenteja

data Person = Person
  { age :: Int,
    name :: Text,
    pets :: [Pet],
    partner :: Person
  } deriving (Show,Generic)

instance HasLentejas Person where
  lentejas = Map.fromList [ 
      ("age", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"age")))),
      ("name", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"name")))),
      -- TODO instances for [] and add the pets field
      ("partner", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"partner"))))
    ]

data Pet = Pet
  { petName :: Text,
    petAge :: Int
  } deriving (Show,Generic)

instance HasLentejas Pet where
  lentejas = Map.fromList [ 
      ("petName", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"petName")))),
      ("petAge", SomeLentejaFrom typeRep (LentejaLens (Lens (field @"petAge"))))
    ]

it :: Person
it =
  let john = Person 41 "John" [fido] sara
      sara = Person 43 "Sara" [] john
      fido = Pet "Fido" 4
   in john

main :: IO ()
main = putStrLn "Hello, Haskell!"
