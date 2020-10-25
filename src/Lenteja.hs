{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module Lenteja where

import Control.Lens
import Control.Lens (ReifiedFold, ReifiedLens')
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Type.Reflection

data Lenteja a b
  = LentejaLens (ReifiedLens' a b)
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

data LentejaError = OpticNotFound Text | OpticsDon'tMatch Text Text

inspect :: forall a. HasLentejas a => a -> NonEmpty Text -> Either LentejaError (LentejaResult String)
inspect a (opticName0 :| _) =
  case Map.lookup opticName0 (lentejas @a) of
    Nothing ->
      Left (OpticNotFound opticName0)
    Just (SomeLentejaFrom rep lenteja) -> 
      Right case lenteja of
        LentejaLens (Lens aLens) -> _lens
        LentejaFold (Fold aFold) -> _fold
