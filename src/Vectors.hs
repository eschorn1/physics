{-# OPTIONS_GHC -Wno-partial-fields -Wno-unused-top-binds #-} -- TODO latter should go away

module Vectors ((^+^), (^-^), (*^), (^*), (^/), (<.>), (><),
  angle, degrees2Radians, iHat, jHat, kHat, magnitude, radians2Degrees,
  toCart, toSphere, Vec, vec, vec2D, xComp, yComp, zComp, zeroV) where

import Text.Printf
import Trig

type R = Double
type Rads = Double


data Vec = Cart3D { xComp :: R, yComp :: R, zComp :: R}
         | Sphere  { magComp :: R, thetaComp :: Rads, phiComp :: Rads} deriving (Eq)
-- maybe rename spherical: https://en.wikipedia.org/wiki/Spherical_coordinate_system
-- or maybe Cart2D, Cart3D, Polar, Sphere

instance Show Vec where
  show (Cart3D x y z) = "Cart3D " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z
  show (Sphere m t p) = "Polar " ++ showDouble m ++ " " ++ showDouble t ++ " " ++ showDouble p

toSphere :: Vec -> Vec
toSphere (Cart3D xx yy zz) = Sphere mag theta phi
  where
    mag = sqrt (xx**2 + yy**2 + zz**2)
    raw_t = radians2Degrees $ atan2 (sqrt (xx**2 + yy**2)) zz
    theta = if raw_t < 0 then raw_t + 360 else raw_t
    raw_p = radians2Degrees $ atan2 yy xx
    phi = if raw_p < 0 then raw_p + 360 else raw_p
toSphere x = x

toCart :: Vec -> Vec
toCart (Sphere mag theta phi) = Cart3D x y z
  where
    x = mag * sinDeg theta * cosDeg phi
    y = mag * sinDeg theta * sinDeg phi
    z = mag * cosDeg theta
toCart x = x

showDouble :: R -> String
showDouble x
  | x < 0       = "(" ++ printf "%+.3e" x ++ ")"
  | otherwise   = printf "%+.3e" x

vec :: R -> R -> R -> Vec
vec = Cart3D


infixl 6 ^+^
(^+^) :: Vec -> Vec -> Vec
Cart3D ax ay az ^+^ Cart3D bx by bz = Cart3D (ax+bx) (ay+by) (az+bz)
_ ^+^ _ = undefined

infixl 6 ^-^
(^-^) :: Vec -> Vec -> Vec
Cart3D ax ay az ^-^ Cart3D bx by bz = Cart3D (ax-bx) (ay-by) (az-bz)
_ ^-^ _ = undefined

infixl 7 *^
(*^) :: R -> Vec -> Vec
c *^ Cart3D ax ay az = Cart3D (c*ax) (c*ay) (c*az)
_ *^ _ = undefined

infixl 7 ^*
(^*) :: Vec -> R -> Vec
Cart3D ax ay az ^* c = Cart3D (c*ax) (c*ay) (c*az)
_ ^* _ = undefined

infixr 7 ^/
(^/) :: Vec -> R -> Vec
Cart3D ax ay az ^/ c = Cart3D (ax/c) (ay/c) (az/c)
_ ^/ _ = undefined

infixr 7 <.>
(<.>) :: Vec -> Vec -> R
Cart3D ax ay az <.> Cart3D bx by bz = ax*bx + ay*by + az*bz
a <.> b = (toCart a) <.> (toCart b)

infixl 7 ><
(><) :: Vec -> Vec -> Vec
Cart3D ax ay az >< Cart3D bx by bz = Cart3D (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)
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

-- angleXY :: Vec -> R
-- angleXY v = result
--   where
--    res = radians2Degrees $ atan2 (yComp v) (xComp v)  -- note: atan2
--    result = if res < 0 then res + 360 else res        -- maybe put this in conversion fns?

angle :: Vec -> Vec -> R  -- in radians; suspect
angle a b = acos ((a <.> b) / (magnitude a * magnitude b))

--polarCoord :: Vec -> (R, R)
--polarCoord v = (magnitude v, angleXY v)

radians2Degrees :: R -> R
radians2Degrees r = (r * 360) / (2 * pi)

degrees2Radians :: R -> R
degrees2Radians d = (d * 2 * pi) / 360