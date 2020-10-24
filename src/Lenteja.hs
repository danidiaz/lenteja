{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lenteja where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Control.Lens


data Lenteja a b = 
    LentejaLens (ReifiedLens' a b)
    | LentejaFold (ReifiedFold a b)
