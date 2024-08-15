{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-home-modules -Wno-unused-imports #-}

module Main (main) where

-- stack repl --only-main
-- :r

-- To write...
--   a. vec = cart | polar
--      show polar

--See https://hackage.haskell.org/package/integration-0.2.1/docs/Numeric-Integration-TanhSinh.html
import Numeric.Integration.TanhSinh as Integration

-- See https://hackage.haskell.org/package/hmatrix-gsl-0.19.0.1
-- sudo apt-get install libgsl0-dev liblapack-dev libatlas-base-dev
-- f [theta] = u * m * g / ((cos theta) + u * (sin theta))
-- fst $ minimize NMSimplex2 1e-3 50 [0.5] f [0.2]
import Numeric.GSL.Minimization

import qualified Ch1
import qualified Constants
import qualified Utils
import qualified Vectors as V
import Trig


main :: IO ()
main = do
  putStrLn $ "\n\n" ++ Ch1.ch1

integrate :: (Double -> Double) -> Double -> Double -> Integration.Result
integrate f start stop = last $ Integration.trap f start stop

quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c = (res1, res2)
  where
    det = sqrt (b * b - 4 * a * c)
    res1 = ((-b) + det) / (2 * a)
    res2 = ((-b) - det) / (2 * a)

norm2 :: Double -> Double -> Double
norm2 x y = sqrt (x * x + y * y)

-- Heavily utilized...
g :: Double
g = Constants.g

