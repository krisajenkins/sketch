{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.FileEmbed
import           Data.Foldable                (traverse_)
import           Data.Scientific              (Scientific)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Text.IO                 as Text
import           GHC.TypeLits                 ()
import qualified Numeric.LinearAlgebra        as LA
import           Numeric.LinearAlgebra.Static (L, matrix, mul)
import           Text.Megaparsec              (ParseError, Token, char, newline,
                                               parse, parseErrorPretty, sepBy)
import           Text.Megaparsec.ByteString   (Parser)
import           Text.Megaparsec.Lexer        (number)

dataParser :: Parser [[Scientific]]
dataParser = (number `sepBy` (char ',')) `sepBy` newline

wineData :: ByteString
wineData = $(embedFile "data/wine.data")

someFunc :: IO ()
someFunc =
  case (parse dataParser "" wineData) of
    Left err -> putStr (parseErrorPretty err)
    Right xs -> traverse_ print . filter (not . null) $ xs

a :: L 3 2
a = matrix [1 .. 6]

b :: L 2 1
b = matrix [1 .. 2]

c :: L 3 1
c = mul a b
