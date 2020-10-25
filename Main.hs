{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
    ( toListOf, view, ReifiedFold(Fold), ReifiedGetter(Getter) )
import Data.Generics.Product.Fields (field)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import GHC.Generics ( Generic )
import Lenteja
    ( HasLentejas(..),
      SomeLentejaFrom(SomeLentejaFrom),
      Lenteja(LentejaFold, LentejaGetter),
      inspect )
import Lenteja.Parser ( parseLensyExp )
import Control.Exception ( throwIO )
import Data.Foldable (traverse_)
import Data.Data (Proxy(Proxy))
import Data.List.NonEmpty ( toList )

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
      [ ("age", SomeLentejaFrom (LentejaGetter (Getter (field @"age")))),
        ("name", SomeLentejaFrom (LentejaGetter (Getter (field @"name")))),
        ("pets", SomeLentejaFrom (LentejaGetter (Getter (field @"pets")))),
        ("partner", SomeLentejaFrom  (LentejaGetter (Getter (field @"partner"))))
      ]

data Pet = Pet
  { petName :: Text,
    petAge :: Int
  }
  deriving (Show, Generic)

instance HasLentejas Pet where
  lentejas =
    Map.fromList
      [ ("petName", SomeLentejaFrom  (LentejaGetter (Getter (field @"petName")))),
        ("petAge", SomeLentejaFrom  (LentejaGetter (Getter (field @"petAge"))))
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
  case inspect (Proxy @Person) (toList opticNames) of
    Left error -> throwIO error
    Right (LentejaGetter (Getter lenteja)) -> 
      do let result = view lenteja it
         putStrLn "A single result:"
         putStrLn result
    Right (LentejaFold (Fold lenteja)) -> 
      do let result = toListOf lenteja it
         putStrLn "Multiple results:"
         traverse_ putStrLn result
  repl

main :: IO ()
main = do
  repl
