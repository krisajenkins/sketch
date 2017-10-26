{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib where

import           Control.Monad.Except                      (MonadError,
                                                            runExceptT,
                                                            throwError)
import           Control.Monad.IO.Class                    (MonadIO)
import           Data.ByteString                           (ByteString)
import           Data.FileEmbed                            (embedFile)
import           Data.Monoid                               ((<>))
import           Data.Scientific                           (Scientific,
                                                            toRealFloat)
import           Debug.Trace
import           GHC.TypeLits                              ()
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy             (def, layout_title,
                                                            line, plot, points,
                                                            re, (.=))
import           Numeric.GSL.Minimization                  (MinimizeMethodD (SteepestDescent, VectorBFGS2),
                                                            minimizeD,
                                                            minimizeVD)
import           Numeric.LinearAlgebra                     (Container, matFunc,
                                                            tr, ( #> ), (<.>))
import qualified Numeric.LinearAlgebra                     as Matrix
import           Numeric.LinearAlgebra.Data                (Matrix, Vector,
                                                            asColumn, asRow,
                                                            cols, cond,
                                                            dropColumns,
                                                            dropRows, rows,
                                                            takeColumns,
                                                            takeRows, (><),
                                                            (|||))
import qualified Numeric.LinearAlgebra.Data                as MData
import           Numeric.LinearAlgebra.Static              (L, dim, matrix, mul)
import qualified Numeric.LinearAlgebra.Static              as Static
import           Text.Megaparsec                           (Dec, ParseError,
                                                            Token, char,
                                                            newline, parse,
                                                            sepBy)
import           Text.Megaparsec.ByteString                (Parser)
import           Text.Megaparsec.Lexer                     (number)

data Error =
  ParseFailed (ParseError (Token ByteString) Dec)
  deriving (Show, Eq)

------------------------------------------------------------
-- Parsing.
rawDataParser :: Parser [[Scientific]]
rawDataParser =
  filter (not . null) <$> (number `sepBy` (char ',')) `sepBy` newline

toMatrix :: [[Scientific]] -> Matrix Double
toMatrix = Matrix.fromLists . (fmap (fmap toRealFloat))

wineData :: ByteString
wineData = $(embedFile "data/wine.data")

wineParser :: Parser (Matrix Double)
wineParser = toMatrix <$> rawDataParser

loadData
  :: (MonadError Error m, MonadIO m)
  => m (Matrix Double)
loadData = do
  either (throwError . ParseFailed) pure (parse wineParser "" wineData)

------------------------------------------------------------
-- Matrices
a :: L 3 2
a = matrix [1 .. 6]

b :: L 2 1
b = matrix [1 .. 2]

c :: L 3 1
c = mul a b

------------------------------------------------------------
-- Linear Regression
initialTheta :: Int -> Vector Double
initialTheta n = MData.vector $ take n $ repeat 0

------------------------------------------------------------
-- Charts
signal :: [Double] -> [(Double, Double)]
signal xs =
  [(x, (sin (x * 3.14159 / 45) + 1) / 2 * (sin (x * 3.14159 / 5))) | x <- xs]

writeChart :: IO ()
writeChart =
  toFile def "example.svg" $ do
    layout_title .= "Amplitude Modulation"
    plot (line "am" [signal [0,(0.5) .. 800]])
    plot (points "am points" (signal [0,7 .. 800]))

------------------------------------------------------------
-- Main
main :: IO ()
main = do
  result <-
    runExceptT $ do
      dataSet <- loadData
      let is n v = cond v n 0 1 0
      let m = rows dataSet
      let f = cols dataSet
      let y :: Vector Double
          y = is 1 $ MData.flatten $ takeColumns 1 dataSet
      let x = (Matrix.col (take m (repeat 1.0)) ||| dropColumns 1 dataSet)
      let trainingX = dropRows 2 x
      let cvX = takeRows 2 x
      let trainingY = MData.fromList $ drop 2 $ MData.toList y
      let cvY = MData.fromList $ take 2 $ MData.toList y
      let theta = initialTheta f
      let finalTheta =
            fst $
            minimizeVD
              VectorBFGS2
              0.0001
              500
              0.0001
              0.1
              (costFn x y)
              (gradFn x y)
              theta
      pure $
        Result
          cvX
          cvY
          finalTheta
          (hypothesis finalTheta cvX)
          (costFn cvX cvY finalTheta)
  print result

data Result = Result
  { crossValidationSet    :: Matrix Double
  , crossValidationResult :: Vector Double
  , finalTheta            :: Vector Double
  , finalHyp              :: Vector Double
  , finalCost             :: Double
  } deriving (Show)

gradFn :: Matrix Double -> Vector Double -> Vector Double -> Vector Double
gradFn x y theta =
  (1.0 / fromIntegral (rows x)) * ((tr x #> ((hypothesis theta x) - y)))

hypothesis :: Vector Double -> Matrix Double -> Vector Double
hypothesis theta x = sigmoid (x #> theta)

costFn :: Matrix Double -> Vector Double -> Vector Double -> Double
costFn x y theta = all
  where
    all :: Double
    all = ((vsum (leftTerm - rightTerm))) / (fromIntegral (rows x))
    h = hypothesis theta x
    leftTerm = (-y) * log (h)
    rightTerm = (1.0 - y) * log (1.0 - h)

vsum :: Vector Double -> Double
vsum = sum . MData.toList

sigmoid
  :: Floating a
  => a -> a
sigmoid term = 1.0 / (1.0 + (exp (-term)))
