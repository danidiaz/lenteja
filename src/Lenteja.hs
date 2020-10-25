{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Lenteja where

import Control.Exception ( Exception )
import Control.Lens
    ( folded, to, ReifiedFold(Fold), ReifiedGetter(Getter) )
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Type.Reflection ( Typeable, TypeRep, typeRep )

data Lenteja a b
  = LentejaGetter (ReifiedGetter a b)
  | LentejaFold (ReifiedFold a b)

data SomeLentejaFrom a where
  SomeLentejaFrom :: (Show b, HasLentejas b) => TypeRep b -> Lenteja a b -> SomeLentejaFrom a

data SomeLenteja where
  SomeLenteja :: (Show a, HasLentejas b) => TypeRep a -> SomeLentejaFrom a -> SomeLenteja

type HasLentejas :: Type -> Constraint
class HasLentejas t where
  lentejas :: Map Text (SomeLentejaFrom t)

instance HasLentejas Int where
  lentejas = Map.empty

instance HasLentejas Text where
  lentejas = Map.empty

instance (Show a, HasLentejas a, Typeable a) => HasLentejas [a] where
  lentejas = Map.fromList [("folded", SomeLentejaFrom (typeRep @a) (LentejaFold (Fold folded)))]

data LentejaResult a
  = SingleResult a
  | MultipleResults [a]
  deriving (Functor)

data LentejaError = OpticNotFound Text deriving (Show)

instance Exception LentejaError

inspect :: forall a proxy. (HasLentejas a, Show a) => proxy a -> [Text] -> Either LentejaError (Lenteja a String)
inspect _ [] = Right (LentejaGetter (Getter (to show)))
inspect _ (opticName : names) =
  case Map.lookup opticName (lentejas @a) of
    Nothing ->
      Left (OpticNotFound opticName)
    Just (SomeLentejaFrom rep lenteja) ->
      case inspect lenteja names of
        Left err -> 
          Left err
        Right restLenteja ->
          Right case (lenteja, restLenteja) of
            (LentejaGetter (Getter current), LentejaGetter (Getter rest)) -> 
              LentejaGetter (Getter (current . rest))
            (LentejaGetter (Getter current), LentejaFold (Fold rest)) -> 
              LentejaFold (Fold (current . rest))
            (LentejaFold (Fold current), LentejaGetter (Getter rest)) -> 
              LentejaFold (Fold (current . rest))
            (LentejaFold (Fold current), LentejaFold (Fold rest)) -> 
              LentejaFold (Fold (current . rest))
              