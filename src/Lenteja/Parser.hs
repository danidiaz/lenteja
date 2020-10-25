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

import Lenteja ()
import Text.Megaparsec
    ( runParser, sepBy1, Parsec, MonadParsec(takeWhile1P, eof) )
import Text.Megaparsec.Char ( space )
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void ( Void )
import Data.Text (Text)
import Control.Exception (throwIO)
import Data.Char ( isLetter )
import Data.List.NonEmpty ( NonEmpty, fromList )

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
