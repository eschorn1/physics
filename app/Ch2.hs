module Ch2 (ch2) where

-- stack repl   ,   :l Ch2

import Utils
import Vectors

ch2 :: [Char]
ch2 = concat [a ++ "\n" | a <- [p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p49,
      p51, p53, p55, p57, p59, p61, p63, p65, p67, p69]]

p25 :: [Char]
p25 = "P25 " ++ result
  where
    res = (-9 :: Double) + 3 - 12 + 4 - 18 + 7 - 24
    result = " displacement = " ++ show res

p27 :: [Char]
p27 = "P27 " ++ result
  where
    res = (vec 0 18 0) ^+^ (vec 25 0 0)
    mag = magnitude res
    ang = angle2D res
    result = " mag=" ++ show mag ++ " ang=" ++ show ang

p29 :: [Char]
p29 = "P29 " ++ result
  where
    net = (vec 0 40 0) ^+^ (vec (-20) 0 0 ) ^+^ (vec2D 60 45) ^+^ (vec 0 50 0)
    mag = magnitude net
    ang = angle2D net
    result = " mag=" ++ show mag ++ " ang=" ++ show ang

p31 :: [Char]
p31 = "P31 " ++ result
  where
    net = (vec2D 2.5 (180-45)) ^+^ (vec2D 4.7 300) ^+^ (vec2D 1.3 205) ^+^
          (vec2D 5.1 0) ^+^ (vec2D 1.7 85) ^+^ (vec2D 7.2 235) ^+^ (vec2D 2.8 10)
    mag = magnitude net
    ang = angle2D net
    result = " mag=" ++ show mag ++ " ang=" ++ show ang

p33 :: [Char]
p33 = "P33 " ++ result
  where
    s = (vec2D 5 40)
    x = xComp s
    y = yComp s
    tot = x + y
    result = " east=" ++ show x ++ " north=" ++ show y ++ " total=" ++ show tot

p35 :: [Char]
p35 = "P35 " ++ result
  where
    v = vec 6 13 0
    mag = magnitude v
    ang = angle2D v
    result = " displacement=" ++ show mag ++ " angle=" ++ show ang

p37 :: [Char]
p37 = "P37 " ++ result
  where
    a = vec2D 10 30
    b = vec2D 5 53
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 210
    result = " a_x=" ++ show (xComp a) ++ "  a_y=" ++ show (yComp a) ++
      "\n     b_x=" ++ show (xComp b) ++ "  b_y=" ++ show (yComp b) ++
      "\n     c_x=" ++ show (xComp c) ++ "  c_y=" ++ show (yComp c) ++
      "\n     d_x=" ++ show (xComp d) ++ "  d_y=" ++ show (yComp d) ++
      "\n     f_x=" ++ show (xComp f) ++ "  f_y=" ++ show (yComp f)

p39 :: [Char]
p39 = "P39 " ++ result
  where
    v = vec2D 7.5 (90-15)
    tot = (xComp v) + (yComp v)
    result = " east=" ++ show (xComp v) ++ " north=" ++ show (yComp v) ++ " total=" ++ show tot

p41 :: [Char]
p41 = "P41 " ++ result
  where
    v = vec2D 5 40
    extra = (xComp v) + (yComp v) - (magnitude v)
    result = " extra=" ++ show extra ++ " east=" ++ show (xComp v) ++ " north=" ++ show (yComp v) ++
      "\n     " ++ show v

p43 :: [Char]
p43 = "P43 " ++ result
  where
    p1 = vec2D 2.5 (radians2Degrees (pi / 6))
    p2 = vec2D 3.8 (radians2Degrees (2 * pi / 3))
    dist = magnitude $ p1 ^-^ p2
    result = " P1=" ++ show p1 ++ "\n     P2=" ++ show p2 ++ "\n     Dist=" ++ show dist

p45 :: [Char]
p45 = "P45 " ++ result
  where
    p1 = vec 2 (-4) 0
    p2 = vec (-3) 3 0
    dist = magnitude (p1 ^-^ p2)
    ang1 = angle p1 iHat
    result = " Dist=" ++ show dist ++ " p1=" ++ show (magnitude p1) ++ " ang=" ++ show ang1 ++
      "\n     p2=" ++ show (polarCoord p2)

p47 :: [Char]
p47 = "P47 " ++ result
  where
    b = ((-1) *^ iHat) ^+^ ((-4) *^ jHat)
    a = ((-3) *^ iHat) ^+^ ((-2) *^ jHat)
    sum1 = a ^+^ b
    ang = angle2D sum1
    result = " A+B=" ++ show sum1 ++ " mag=" ++ show (magnitude sum1) ++ " angle=" ++ show ang

p49 :: [Char]
p49 = "P49 " ++ result
  where
    a = vec 3 (-4) 4
    b = vec 2 3 (-7)
    c = a ^+^ b
    d = (2 *^ a) ^-^ b
    result = " c=" ++ show c ++ " mag=" ++ show (magnitude c) ++ "\n     d=" ++ show d ++ " mag=" ++ show (magnitude d)

p51 :: [Char]
p51 = "P51 " ++ result
  where
    d = (vec2D 2.5 135) ^+^ (vec2D 4.7 300) ^+^ (vec2D 1.3 205) ^+^ (vec2D 5.1 0) ^+^
        (vec2D 1.7 85) ^+^ (vec2D 7.2 235) ^+^ (vec2D 2.8 10)
    result = " d=" ++ show d ++ "\n     mag=" ++ show (magnitude d) ++ " ang=" ++ show (angle2D d)

p53 :: [Char]
p53 = "P53 " ++ result
  where
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 (180+30)
    ra = f ^-^ d
    rb = ((3 *^ f) ^+^ (2 *^ d) ^-^ c) ^/ 5
    result = " ra =" ++ show ra ++ "\n     rb=" ++ show rb

p55 :: [Char]
p55 = "P55 " ++ result
  where
    d = vec (3 + 1 - 2) (2 + 1) 0
    d100 = d ^* 100
    result = " disp=" ++ show d100 ++ " mag=" ++ show (magnitude d100) ++ " ang=" ++ show (angle2D d100)

p57 :: [Char]
p57 = "P57 " ++ result
  where
   d = vec 3 (-4) 0
   r = ((-1) *^ d) ^+^ ((-4) * (magnitude d) *^ jHat )
   result = " d=" ++ show r

p59 :: [Char]
p59 = "P59 " ++ result
  where
    eHat = vec (1 / sqrt 5) (-2 / sqrt 5) 0
    e = 400 *^ (eHat ^/ (magnitude eHat))
    ang = angle2D e
    result = " vec=" ++ show e ++ "\n     mag=" ++ show (magnitude e) ++ " ang=" ++ show ang

p61 :: [Char]
p61 = "P61 " ++ result
  where
    boeing_z = 2500 -- * cos (degrees2Radians 10)
    boeing_xy = 2500 * cos (degrees2Radians 10) / sin (degrees2Radians 10)
    boeing_x = boeing_xy * cos (degrees2Radians 150)
    boeing_y = boeing_xy * sin (degrees2Radians 150)
    boeing = vec boeing_x boeing_y boeing_z
    dc3_z = 3000
    dc3_x = dc3_z * cos (degrees2Radians 5) / sin (degrees2Radians 5)
    dc3 = vec (-dc3_x) 0 dc3_z
    rbd = boeing ^-^ dc3
    result = " b=" ++ show boeing ++ "\n     dc3=" ++ show dc3 ++ " b_mag=" ++
      show (magnitude boeing) ++ "\n     rbd" ++ show rbd ++ " mag=" ++ show (magnitude rbd)

p63 :: [Char]
p63 = "P63 " ++ result
  where
    -- a = vec2D 10 30
    --c = vec2D 12 300
    -- note these are at right angles to each other so answer to a) and b) are zero
    f = vec2D 20 210
    result = " c=" ++ show (cos (degrees2Radians 210)) ++ " d=" ++ show (xComp f)

p65 :: [Char]
p65 = "P65 " ++ result
  where
    d = vec 2 (-4) 1
    dx = radians2Degrees $ acos ((xComp d) / (magnitude d))
    dy = radians2Degrees $ acos ((yComp d) / (magnitude d))
    dz = radians2Degrees $ acos ((zComp d) / (magnitude d))
    result = " x_ang=" ++ show dx ++ " y_ang=" ++ show dy ++ " z_ang=" ++ show dz

p67 :: [Char]
p67 = "P67 " ++ result
  where
    a = vec2D 10 30
    b = vec2D 5 53
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 210
    result = " a=" ++ show (a >< c) ++ "\n     b=" ++ show (a >< f) ++ "\n     c=" ++ show (d >< c) ++
      "\n     d=" ++ show (a >< (f ^+^ (2 *^ c))) ++ "\n     e=" ++ show (iHat >< b) ++
      "\n     f=" ++ show (jHat >< b) ++
      "\n     g=" ++ show ((vec 3 (-1) 0) >< b) ++
      "\n     h=" ++ show (b><b)

p69 :: [Char]
p69 = "P69 " ++ result
  where
    a = vec2D 10 30
    b = vec2D 5 53
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 210
    ans_a = (a >< f) <.> d
    ans_b = (a >< f) <.> (d >< b)
    ans_c = (a <.> f) *^ (d >< b)
    result = " a=" ++ show ans_a ++ "\n     b=" ++ show ans_b ++ "\n     c=" ++ show ans_c