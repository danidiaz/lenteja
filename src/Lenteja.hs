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

import Control.Exception
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
  deriving (Functor)

data LentejaError = OpticNotFound Text | OpticsDon'tMatch Text Text deriving (Show)

instance Exception LentejaError

inspect :: forall a. HasLentejas a => a -> NonEmpty Text -> Either LentejaError (LentejaResult String)
inspect a (opticName0 :| _) =
  case Map.lookup opticName0 (lentejas @a) of
    Nothing ->
      Left (OpticNotFound opticName0)
    Just (SomeLentejaFrom rep lenteja) ->
      let result =
            case lenteja of
              LentejaLens (Lens aLens) ->
                SingleResult (view aLens a)
              LentejaFold (Fold aFold) ->
                MultipleResults (toListOf aFold a)
       in Right (show <$> result)