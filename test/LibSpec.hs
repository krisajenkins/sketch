module LibSpec where

import           Debug.Trace
import           Lib                        (sigmoid)
import           Numeric.LinearAlgebra      (( #> ))
import           Numeric.LinearAlgebra.Data (Matrix, Vector, cond, tr, vector,
                                             (><))
import           Test.Hspec                 hiding (Spec)
import           Test.Hspec                 as Hspec
import           Test.HUnit                 (Assertion, assertBool)

spec :: Hspec.Spec
spec = do
  describe "sigmoid" $
    it "Should be correct." $ do
      (sigmoid ((3 >< 2) [1.0 ..]) :: Matrix Double) `shouldBe`
        (((3 >< 2)
            [ 0.7310585786300049
            , 0.8807970779778823
            , 0.9525741268224334
            , 0.9820137900379085
            , 0.9933071490757153
            , 0.9975273768433653
            ]) :: Matrix Double)
  describe "cond behaviour" $
    it "Should be correct." $ do
      (traceShowId (cond ((3 >< 2) [1.0 ..] :: Matrix Double) 3 0 1 0)) `shouldBe`
        ((3 >< 2) [0, 0, 1, 0, 0, 0] :: Matrix Double)
  describe "m.v behaviour" $
    it "Should be correct." $ do
      let x = (3 >< 2) [1.0 ..] :: Matrix Double
          theta = vector [10.0, 5.0] :: Vector Double
      (x #> theta) `shouldBe` (vector [20.0, 50.0, 80.0] :: Vector Double)
