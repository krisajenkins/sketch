{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib where

import           Control.Monad.Except                      (MonadError,
                                                            throwError)
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString                           as BS
import           Data.FileEmbed
import           Data.Foldable                             (traverse_)
import           Data.Scientific                           (Scientific)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Encoding                        (decodeUtf8)
import qualified Data.Text.IO                              as Text
import           GHC.TypeLits                              ()
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy             (def, layout_title,
                                                            line, plot, points,
                                                            (.=))
import qualified Numeric.LinearAlgebra                     as LA
import           Numeric.LinearAlgebra.Static              (L, matrix, mul)
import           Text.Megaparsec                           (Dec, ParseError,
                                                            Token, char,
                                                            newline, parse,
                                                            parseErrorPretty,
                                                            sepBy)
import           Text.Megaparsec.ByteString                (Parser)
import           Text.Megaparsec.Lexer                     (number)

dataParser :: Parser [[Scientific]]
dataParser = (number `sepBy` (char ',')) `sepBy` newline

wineData :: ByteString
wineData = $(embedFile "data/wine.data")

data Error =
  ParseFailed (ParseError (Token ByteString) Dec)
  deriving (Show, Eq)

mParse
  :: MonadError Error m
  => Parser a -> ByteString -> m a
mParse parser input =
  case (parse parser "" input) of
    Left err -> throwError $ ParseFailed err
    Right xs -> pure xs

signal :: [Double] -> [(Double, Double)]
signal xs =
  [(x, (sin (x * 3.14159 / 45) + 1) / 2 * (sin (x * 3.14159 / 5))) | x <- xs]

someFunc :: IO ()
someFunc = do
  case (parse dataParser "" wineData) of
    Left err -> putStr (parseErrorPretty err)
    Right xs -> traverse_ print . filter (not . null) $ xs
  toFile def "example.svg" $ do
    layout_title .= "Amplitude Modulation"
    plot (line "am" [signal [0,(0.5) .. 800]])
    plot (points "am points" (signal [0,7 .. 800]))

a :: L 3 2
a = matrix [1 .. 6]

b :: L 2 1
b = matrix [1 .. 2]

c :: L 3 1
c = mul a b
