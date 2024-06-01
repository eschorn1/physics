module Ch1 (ch1) where

-- stack repl   ,   :l Ch2

import Utils
import Vectors

ch1 :: String
ch1 = concat [a ++ "\n" | a <- [p1, p3, p5, p7, p9,
  p11, p13, p15, p17, p19, p21, p23, p25, p27, p29,
  p31, p33, p35, p37, p39]]


p1 :: String
p1 = prettyPrint "P1.1  " " a=" a
  where
    avg_density mass volume = mass / volume
    a = avg_density 5.97e+24 (4 * pi * 6.37e+6**3 / 3) :: Double
    -- iron with a bunch of water
    -- more dense than granite


p3 :: String
p3 = prettyPrint "P1.3  " " a=" s2_rad
  where
    sphere_vol radius = 4 * pi * radius**3 / 3
    sphere_rad volume = (3 *volume / (4 * pi))**(1/3)
    s1_vol = sphere_vol 0.045
    s2_vol = s1_vol * 5
    s2_rad = sphere_rad s2_vol :: Double


p5 :: String
p5 = prettyPrint "P1.5  " " visible angle is much less than 3e-4= " theta
  where
    -- assume directly overhead, then right triangle with x=7m, h=200km
    theta = asin (7 / 200e3) :: Double


p7 :: String
p7 = prettyPrint "P1.7  " " d=" a
 where
   hyp = 0.2
   -- a^2 + a^2 = hyp^2
   a = sqrt ( hyp**2 / 2) :: Double


p9 :: String
p9 = prettyPrint "P1.9   b is dimensionally correct"


p11 :: String
p11 = prettyPrint "P1.11 " " a=" density
  where
    mass = 23.94e-3 -- g -> kg
    vol = 2.10 / 100**3
    density = mass / vol :: Double


p13 :: String
p13 = prettyPrint "P1.13 " " a=" r
  where
    iron = 7.86e3 * 2**3
    --alum = 2.70e3 * r**3
    r = (iron / 2.7e3)**(1/3) :: Double


p15 :: String
p15 = prettyPrint "P1.15 " " a=" thickness " meters"
  where
    vol = 3.78e-3
    area = 25
    thickness = vol / area :: Double


p17 :: String
p17 = prettyPrint "P1.17 " " a=" bathtub " or 1e2 100 kg; 1e3 kg copper " copper
  where
    -- water density = 1e3 kg/m3
    -- bathtub = 40cm * 200cm * 20cm
    bathtub = 0.4*2*0.2 * 1e3 ::Double -- in kg
    copper = 0.4*2*0.2 * 8.92e3 :: Double


p19 :: String
p19 = prettyPrint "P1.19 " " num per square kilomerter =" num
 where
   au = 1.496e11
   vol = 4 * (1 * au * 1 * au)
   num = 1e9 * 10e9 / vol :: Double


p21 :: String
p21 = prettyPrint "P1.21 " " secs/yr=" ( show secs )
  where
    secs = 365.242199 * 24 * 60 * 60 :: Double


p23 :: String
p23 = prettyPrint "P1.23 " " a=" res
  where
    -- car / suv = 1.94; car = 1.94 * suv
    -- car - suv = 18; 1.94 * suv - suv = 18
    res = 18 / 0.947 :: Double


p25 :: String
p25 = prettyPrint "P1.25 " " a=" sparrows
  where
    -- sparrows / birds = 2.25
    -- 91 total; #sparrows?
    sparrows = 2.25 * birds
    -- 2.25 * birds + birds = 91
    birds = 91 / 3.25 :: Double


p27 :: String
p27 = prettyPrint "P1.27 " " a=" res
  where
    -- 0.5 * qsr + 0.5 + qss = 0.5 * qtt
    -- sr + ss = tt; s (r+s) = tt
    -- 3qr = qs; 3r = s
    -- 3rr + 3r3r = tt; 3r^2 + 9r^2 = t^2; 12r^2 = t^2; t/r = sqrt 12
    res = sqrt 12 :: Double


p29 :: String
p29 = prettyPrint "P1.29  " (100 * sqrt 10 :: Double)


p31 :: String
p31 = prettyPrint "P1.31  " num
  where
    vol_galaxy = pi * 1e21**2 * 1e19
    vol_sun = 4 * pi * 4e16**3 / 3
    num = vol_galaxy / vol_sun :: Double

p33 :: String
p33 = prettyPrint "P1.33  b=" (water_mass * proportion)
  where
    -- length = 1e-6
    -- single_vol = length ** 3 -- assume cube
    -- weight_one = single_vol * 1e3 -- density of 1kg/m3 same as water
    water_mass = 6e23 -- 10% mass of earth
    proportion =  1e-9 :: Double -- proportion of water sample


p35 :: String
p35 = prettyPrint "P1.35  a=" a
  where
    spherical_volume radius = 4 * pi * radius**3 / 4
    a = (spherical_volume 7.4) - (spherical_volume 6.5) :: Double

p37 :: String
p37 = prettyPrint "P1.37 " " c1=" c1 " c2=" c2 " WRONG "
  where
    -- v1 t = 1.5 * t + 0.008 * t**2 -- 1e6 cf/month
    scale = (30*24*60*60) / 1e6
    c1 = 1.5 * scale :: Double
    c2 = 0.008 * scale :: Double

p39 :: String
p39 = prettyPrint "P1.39 " " a= d tan theta tan phi / (tan phi - tan theta)"
 -- tan theta = y / x
 -- tan phi = y / (x - d)
 -- (x - d) tan phi = x tan theta
 -- x tan phi - x tan theta = d tan phi
 -- x (tan phi - tan theta) = d tan phi
 -- x = d tan phi / (tan phi - tan theta)
 -- y = d tan theta tan phi / (tan phi - tan theta)