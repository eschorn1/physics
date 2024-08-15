module Trig (cosDeg, degrees2Radians, radians2Degrees, sinDeg, tanDeg) where

radians2Degrees :: Double -> Double
radians2Degrees r = (r * 360) / (2 * pi)

degrees2Radians :: Double -> Double
degrees2Radians d = (d * 2 * pi) / 360

cosDeg :: Double -> Double
cosDeg = cos . degrees2Radians

sinDeg :: Double -> Double
sinDeg = sin . degrees2Radians

tanDeg :: Double -> Double
tanDeg x = sinDeg x / cosDeg x
