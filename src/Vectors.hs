{-# OPTIONS_GHC -Wno-partial-fields -Wno-unused-top-binds #-} -- TODO latter should go away

module Vectors ((^+^), (^-^), (*^), (^*), (^/), (<.>), (><),
  angle, angleXY, degrees2Radians, iHat, jHat, kHat, magnitude, polarCoord, radians2Degrees,
  Vec, vec, vec2D, xComp, yComp, zComp, zeroV) where

import Text.Printf

type R = Double
type Rads = Double


data Vec = Cart  { xComp :: R, yComp :: R, zComp :: R}
         | Polar { val1a :: R, val2a :: Rads, val3a :: Rads} deriving (Eq)

instance Show Vec where
  show (Cart x y z) = "vec " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z
  show (Polar _ _ _) = undefined

showDouble :: R -> String
showDouble x
  | x < 0       = "(" ++ printf "%+.3e" x ++ ")"
  | otherwise   = printf "%+.3e" x

vec :: R -> R -> R -> Vec
vec = Cart


infixl 6 ^+^
(^+^) :: Vec -> Vec -> Vec
Cart ax ay az ^+^ Cart bx by bz = Cart (ax+bx) (ay+by) (az+bz)
_ ^+^ _ = undefined

infixl 6 ^-^
(^-^) :: Vec -> Vec -> Vec
Cart ax ay az ^-^ Cart bx by bz = Cart (ax-bx) (ay-by) (az-bz)
_ ^-^ _ = undefined

infixl 7 *^
(*^) :: R -> Vec -> Vec
c *^ Cart ax ay az = Cart (c*ax) (c*ay) (c*az)
_ *^ _ = undefined

infixl 7 ^*
(^*) :: Vec -> R -> Vec
Cart ax ay az ^* c = Cart (c*ax) (c*ay) (c*az)
_ ^* _ = undefined

infixr 7 ^/
(^/) :: Vec -> R -> Vec
Cart ax ay az ^/ c = Cart (ax/c) (ay/c) (az/c)
_ ^/ _ = undefined

infixr 7 <.>
(<.>) :: Vec -> Vec -> R
Cart ax ay az <.> Cart bx by bz = ax*bx + ay*by + az*bz
_ <.> _ = undefined

infixl 7 ><
(><) :: Vec -> Vec -> Vec
Cart ax ay az >< Cart bx by bz = Cart (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)
_ >< _ = undefined


vec2D :: R -> R -> Vec
vec2D r degrees = vec (r * cos (degrees2Radians degrees)) (r * sin (degrees2Radians degrees)) 0

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0

magnitude :: Vec -> R
magnitude v = sqrt $ (xComp v)^(2 :: Integer) + (yComp v)^(2 :: Integer) + (zComp v)^(2 :: Integer)

angleXY :: Vec -> R
angleXY v = result
  where
    res = radians2Degrees $ atan2 (yComp v) (xComp v)  -- note: atan2
    result = if res < 0 then res + 360 else res        -- maybe put this in conversion fns?

angle :: Vec -> Vec -> R  -- in radians; suspect
angle a b = acos ((a <.> b) / (magnitude a * magnitude b))

polarCoord :: Vec -> (R, R)
polarCoord v = (magnitude v, angleXY v)

radians2Degrees :: R -> R
radians2Degrees r = (r * 360) / (2 * pi)

degrees2Radians :: R -> R
degrees2Radians d = (d * 2 * pi) / 360