{-# OPTIONS_GHC -Wno-partial-fields -Wno-unused-top-binds #-} -- TODO latter should go away

module Vectors (add, angle, cart2d, cross, degrees2Radians, divS, dot, magnitude, mulS,
                polar, radians2Degrees, sMul, sub, toCart, toSphere, Vec (Cart3D, Sphere),
                xHat, yHat, zero, zHat) where


import Text.Printf
import Trig as T


type R = Double
type Degrees = Double


data Vec = Cart3D { xComp :: R, yComp :: R, zComp :: R}
         | Sphere { magComp :: R, thetaComp :: Degrees, phiComp :: Degrees} deriving (Eq)

polar :: R -> R -> Vec
polar mag theta = Sphere mag 90 theta


cart2d :: R -> R -> Vec
cart2d x y = Cart3D x y 0


instance Show Vec where
  show (Cart3D x y z) = "Cart3D " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z
  show (Sphere m t p) = "Sphere " ++ showDouble m ++ " " ++ showAngle t ++ " " ++ showAngle p


showDouble :: R -> String
showDouble x
  | x < 0       = "(" ++ printf "%+.3e" x ++ ")"
  | otherwise   = printf "%+.3e" x


showAngle :: R -> String
showAngle x = printf "%3f" x


toSphere :: Vec -> Vec
toSphere (Cart3D xx yy zz) = Sphere mag theta phi
  where
    mag = sqrt (xx**2 + yy**2 + zz**2)
    raw_t = T.radians2Degrees $ atan2 (sqrt (xx**2 + yy**2)) zz
    theta = if raw_t < 0 then raw_t + 360 else raw_t
    raw_p = T.radians2Degrees $ atan2 yy xx
    phi = if raw_p < 0 then raw_p + 360 else raw_p
toSphere x = x


toCart :: Vec -> Vec
toCart (Sphere mag theta phi) = Cart3D x y z
  where
    x = mag * T.sinDeg theta * T.cosDeg phi
    y = mag * T.sinDeg theta * T.sinDeg phi
    z = mag * T.cosDeg theta
toCart x = x


infixl 6 `add`
add :: Vec -> Vec -> Vec
Cart3D ax ay az `add` Cart3D bx by bz = Cart3D (ax+bx) (ay+by) (az+bz)
a `add` b = (toCart a) `add` (toCart b)


infixl 6 `sub`
sub :: Vec -> Vec -> Vec
Cart3D ax ay az `sub` Cart3D bx by bz = Cart3D (ax-bx) (ay-by) (az-bz)
a `sub` b = (toCart a) `sub` (toCart b)


infixl 7 `sMul`
sMul :: R -> Vec -> Vec
s `sMul` Cart3D ax ay az = Cart3D (s*ax) (s*ay) (s*az)
s `sMul` b = s `sMul` (toCart b)


infixl 7 `mulS`
mulS :: Vec -> R -> Vec
Cart3D ax ay az `mulS` s = Cart3D (s*ax) (s*ay) (s*az)
a `mulS` s = (toCart a) `mulS` s


infixr 7 `divS`
divS :: Vec -> R -> Vec
Cart3D ax ay az `divS` b = Cart3D (ax/b) (ay/b) (az/b)
a `divS` b = (toCart a) `divS` b


infixr 7 `dot`
dot :: Vec -> Vec -> R
Cart3D ax ay az `dot` Cart3D bx by bz = ax*bx + ay*by + az*bz
a `dot` b = (toCart a) `dot` (toCart b)


infixl 7 `cross`
cross :: Vec -> Vec -> Vec
Cart3D ax ay az `cross` Cart3D bx by bz = Cart3D (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)
a `cross` b = (toCart a) `cross` (toCart b)


xHat :: Vec
xHat = Cart3D 1 0 0


yHat :: Vec
yHat = Cart3D 0 1 0


zHat :: Vec
zHat = Cart3D 0 0 1


zero :: Vec
zero = Cart3D 0 0 0


magnitude :: Vec -> R
magnitude (Sphere m _ _) = m
magnitude a = magnitude (toSphere a)


angle :: Vec -> Vec -> R
angle a b = degs
  where
    rads = (a `dot` b) / (magnitude a * magnitude b)
    lim = acos $ max (min 1 rads) (-1) -- handle 'bad' rounding
    degs = T.radians2Degrees lim


