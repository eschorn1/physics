module Vectors ((^+^), (^-^), (*^), (^*), (^/), (<.>), (><),
  iHat, jHat, kHat, Vec, vec, xComp, yComp, zComp, zeroV) where

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

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0
