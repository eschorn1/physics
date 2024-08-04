{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

-- stack repl
-- :r

import Ch1
import Vectors

main :: IO ()
main = do
  putStrLn $ "\n\n" ++ ch1


quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c = (res1, res2)
  where
    det = sqrt (b * b - 4 * a * c)
    res1 = ((-b) + det) / (2 * a)
    res2 = ((-b) - det) / (2 * a)

norm2 :: Double -> Double -> Double
norm2 x y = sqrt (x * x + y * y)

cosDeg :: Double -> Double
cosDeg = cos . degrees2Radians

sinDeg :: Double -> Double
sinDeg = sin . degrees2Radians

tanDeg :: Double -> Double
tanDeg x = sinDeg x / cosDeg x
