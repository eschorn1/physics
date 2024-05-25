module Ch3 (ch3) where

-- stack repl   ,   :l Ch3

import Utils
import Vectors

ch3 :: String
ch3 = concat [a ++ "\n" | a <- [p25, p27, p29, p35, p37, p41, p43, p45, p49,
             p51, p53, p55]] -- , p57, p59, p61, p63, p65, p67, p69]]


p25 :: String
p25 = prettyPrint "P3.25 " " a=(-2,5)*iHat" "  b=7*iHat"


p27 :: String
p27 = prettyPrint "P3.27 " " a=2sec" " b.disp=" disp
  where
    x t = 4.0 - 2.0 * t
    disp = (x 6) - (x 3) :: Double


p29 :: String
p29 = prettyPrint "P3.29 " " a.vel=" vel " b. about triple"
  where
    xDist = 23.5e3 / sin (degrees2Radians 10)
    vel = xDist / 150


p35 :: String
p35 = prettyPrint "P3.35 " " a=(" v2 "," v3 ")  b= " (abs v2) "," (abs v3) ") c=" $ (v2+v3) / 2
  where
    -- x t = 10 * t - 2 * t * t
    v t = 10 - 4 * t :: Double
    v2 = v 2.0
    v3 = v 3.0


p37 :: String
p37 = prettyPrint "P3.37 " " a=" a
  where
    v0 = 0
    v7 = 30
    a = (v7 - v0) / 7 :: Double


p41 :: String
p41 = prettyPrint "P3.41 " " a=" a " which is " comp "g "
  where
    v0  = 0
    v60 = 6.5e3
    a = (v60 - v0) / 60 :: Double
    comp = a / 9.8


p43 :: String
p43 = prettyPrint "P3.43 " " disp=" disp
  where
    disp = 30 * 5 :: Double


p45 :: String
p45 = prettyPrint "P3.45 " " a=" (disp 5) " b=" (vel 5)
  where
    vi = 30
    a = 30
    disp t = vi * t + 0.5 * a * t^(2 :: Integer)
    vel t = vi + a * t :: Double


p49 :: String
p49 = prettyPrint "P3.49 " " a=" a " b=" vi_0 " c=" t
  where
    delta_t = 20 - 10
    vi = 5
    vf = -8
    a = (vf - vi) / delta_t :: Double
    vi_0 = vi - a * 10
    -- v = vi + a * t = 0 ; so t = -vi / a
    t = (-vi_0) / a


p51 :: String
p51 = prettyPrint "P3.51 " " vf=" vf
  where
    a = 6.2e5
    t = 8.1e-4
    vf = a * t :: Double


p53 :: String
p53 = prettyPrint "P3.53 " " c=" (x tf) " d = " (v tf)
  where
    x0 = 0
    v0 = 0
    a = 2.4
    tf = 12
    x t = x0 + v0 * t + 0.5 * a * t * t :: Double
    v t = v0 + a * t


p55 :: String
p55 = prettyPrint "P3.55 " " t=" t " a=" a
  where
    v0 = 0
    vf = 30
    xf = 1.8
    -- vf = v0 + a*t; 30 = a*t; a = 30/t
    -- xf = x0 + v0*t + 0.5*a*t^2; 1.8 = 0.5*a*t^2; 1.8 = 0.5 * a*t * t; 1.8 = 30 * t
    t = 2 * 1.8 / 30 :: Double
    a = 30 / t