{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lenteja.Parser where

import Lenteja
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void
import Data.Text (Text)
import Control.Exception (throwIO)
import Data.Char
import Data.List.NonEmpty

type Parser = Parsec Void Text

parseLensyExp :: Text -> IO (NonEmpty Text)
parseLensyExp text = do
    let r = runParser lensyExpP "" text
    case r of
        Left err -> throwIO err
        Right optics -> pure optics

lensyExpP :: Parser (NonEmpty Text)
lensyExpP = fromList <$> sepBy1 opticP dotP <* eof

opticP :: Parser Text
opticP = takeWhile1P Nothing isLetter <* space

dotP :: Parser ()
dotP = L.symbol space "." *> pure ()
