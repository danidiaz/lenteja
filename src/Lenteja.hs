{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lenteja where

import Control.Lens ( ReifiedLens', ReifiedFold )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Type.Reflection ( TypeRep )
import Data.Kind ( Type, Constraint )

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
