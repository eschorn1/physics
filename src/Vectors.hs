module Vectors ((^+^), (^-^), (*^), (^*), (^/), (<.>), (><),
  angle, angle2D, degrees2Radians, iHat, jHat, kHat, magnitude, polarCoord, radians2Degrees,
  Vec, vec, vec2D, xComp, yComp, zComp, zeroV) where

type R = Double

infixl 6 ^+^
(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax+bx) (ay+by) (az+bz)

infixl 6 ^-^
(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax-bx) (ay-by) (az-bz)

infixl 7 *^
(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c*ax) (c*ay) (c*az)

infixl 7 ^*
(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c*ax) (c*ay) (c*az)

infixr 7 ^/
(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax/c) (ay/c) (az/c)

infixr 7 <.>
(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax*bx + ay*by + az*bz

infixl 7 ><
(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz = Vec (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

data Vec = Vec { xComp :: R, yComp :: R, zComp :: R} deriving (Eq)

instance Show Vec where
  show (Vec x y z) = "vec " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z

showDouble :: R -> String
showDouble x
  | x < 0       = "(" ++ show x ++ ")"
  | otherwise   = show x

vec :: R -> R -> R -> Vec
vec = Vec

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

angle2D :: Vec -> R
angle2D v = result
  where
    res = radians2Degrees $ atan2 (yComp v) (xComp v)  -- note: atan2
    result = if res < 0 then res + 360 else res        -- maybe put this in conversion fns?

angle :: Vec -> Vec -> R  -- in radians
angle a b = acos ((a <.> b) / (magnitude a * magnitude b))

polarCoord :: Vec -> (R, R)
polarCoord v = (magnitude v, angle2D v)

radians2Degrees :: R -> R
radians2Degrees r = (r * 360) / (2 * pi)

degrees2Radians :: R -> R
degrees2Radians d = (d * 2 * pi) / 360