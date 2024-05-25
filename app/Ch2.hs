module Ch2 (ch2) where

-- stack repl   ,   :l Ch2

import Utils
import Vectors

ch2 :: String
ch2 = concat [a ++ "\n" | a <- [p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p49,
      p51, p53, p55, p57, p59, p61, p63, p65, p67, p69]]


p25 :: String
p25 = prettyPrint "P2.25 " " disp=" disp " dist=" (abs disp)
  where
    disp = (-9 :: Double) + 3 - 12 + 4 - 18 + 7 - 24


p27 :: String
p27 = prettyPrint "P2.27 " " mag=" mag " ang=" ang
  where
    res = (vec (-18) 0 0) ^+^ (vec 0 25 0)
    mag = magnitude res
    ang = angleXY res


p29 :: String
p29 = prettyPrint "P2.29 " " mag=" mag " ang=" ang
  where
    net = (vec 0 40 0) ^+^ (vec (-20) 0 0 ) ^+^ (vec2D 60 45) ^+^ (vec 0 50 0)
    mag = magnitude net
    ang = angleXY net


p31 :: String
p31 = prettyPrint "P2.31 " " mag=" mag " ang=" ang
  where
    net = (vec2D 2.5 (180-45)) ^+^ (vec2D 4.7 300) ^+^ (vec2D 1.3 205) ^+^
          (vec2D 5.1 0) ^+^ (vec2D 1.7 85) ^+^ (vec2D 7.2 235) ^+^ (vec2D 2.8 10)
    mag = magnitude net
    ang = angleXY net


p33 :: String
p33 = prettyPrint "P2.33 " " east=" x " north=" y " total=" tot
  where
    s = (vec2D 5 40)
    x = xComp s
    y = yComp s
    tot = x + y


p35 :: String
p35 = prettyPrint "P2.35 " " mag=" mag " ang=" ang
  where
    v = vec 6 13 0
    mag = magnitude v
    ang = angleXY v


p37 :: String
p37 = prettyPrint "P2.37 " " a_x=" (xComp a) "  a_y=" (yComp a)
                    "\n       b_x=" (xComp b) "  b_y=" (yComp b)
                    "\n       c_x=" (xComp c) "  c_y=" (yComp c)
                    "\n       d_x=" (xComp d) "  d_y=" (yComp d)
                    "\n       f_x=" (xComp f) "  f_y=" (yComp f)
  where
    a = vec2D 10 30
    b = vec2D 5 53
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 210


p39 :: String
p39 = prettyPrint "P2.39 " " east=" (xComp v) " north=" (yComp v) " total=" tot
  where
    v = vec2D 7.5 (90-15)
    tot = (xComp v) + (yComp v)


p41 :: String
p41 = prettyPrint "P2.41 " " extra=" extra " east=" (xComp v) " north=" (yComp v) "\n       " v
  where
    v = vec2D 5 40
    extra = (xComp v) + (yComp v) - (magnitude v)


p43 :: String
p43 = prettyPrint "P2.43 " " P1=" p1 "\n       P2=" p2 "\n       Dist=" dist
  where
    p1 = vec2D 2.5 (radians2Degrees (pi / 6))
    p2 = vec2D 3.8 (radians2Degrees (2 * pi / 3))
    dist = magnitude $ p1 ^-^ p2


p45 :: String  -- TODO: need toPolar then show
p45 = prettyPrint "P2.45 " " Dist=" dist " p1=" (magnitude p1) " ang=" ang1 "\n       p2=" (show $ polarCoord p2)
  where
    p1 = vec 2 (-4) 0
    p2 = vec (-3) 3 0
    dist = magnitude (p1 ^-^ p2)
    ang1 = angle p1 iHat


p47 :: String
p47 = prettyPrint "P2.47 " " A+B=" sum1 " mag=" (magnitude sum1) " angle=" ang
  where
    b = ((-1) *^ iHat) ^+^ ((-4) *^ jHat)
    a = ((-3) *^ iHat) ^+^ ((-2) *^ jHat)
    sum1 = a ^+^ b
    ang = angleXY sum1


p49 :: String
p49 = prettyPrint "P2.49 " " c=" c " mag=" (magnitude c) "\n       d=" d " mag=" (magnitude d)
  where
    a = vec 3 (-4) 4
    b = vec 2 3 (-7)
    c = a ^+^ b
    d = (2 *^ a) ^-^ b


p51 :: String
p51 = prettyPrint "P2.51 " " d=" d "\n       mag=" (magnitude d) " ang=" (angleXY d)
  where
    d = (vec2D 2.5 135) ^+^ (vec2D 4.7 300) ^+^ (vec2D 1.3 205) ^+^ (vec2D 5.1 0) ^+^
        (vec2D 1.7 85) ^+^ (vec2D 7.2 235) ^+^ (vec2D 2.8 10)


p53 :: String
p53 = prettyPrint "P2.53 " " ra=" ra "\n       rb=" rb
  where
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 (180+30)
    ra = f ^-^ d
    rb = ((3 *^ f) ^+^ (2 *^ d) ^-^ c) ^/ 5


p55 :: String
p55 = prettyPrint "P2.55 " " disp=" d100 " mag=" (magnitude d100) " ang=" (angleXY d100)
  where
    d = vec (3 + 1 - 2) (2 + 1) 0
    d100 = d ^* 100


p57 :: String
p57 = prettyPrint "P2.57 " " d=" r
  where
   d = vec 3 (-4) 0
   r = ((-1) *^ d) ^+^ ((-4) * (magnitude d) *^ jHat )


p59 :: String
p59 = prettyPrint "P2.59 " " vec=" e "\n       mag=" (magnitude e) " ang=" ang
  where
    eHat = vec (1 / sqrt 5) (-2 / sqrt 5) 0
    e = 400 *^ (eHat ^/ (magnitude eHat))
    ang = angleXY e


p61 :: String
p61 = prettyPrint "P2.61 " " b=" boeing "\n       dc3=" dc3 " b_mag=" (magnitude boeing)
                   "\n       rbd" rbd " mag=" (magnitude rbd)
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


p63 :: String
p63 = prettyPrint "P2.63 " " c=" (cos (degrees2Radians 210)) " d=" (xComp f)
  where
    f = vec2D 20 210


p65 :: String
p65 = prettyPrint "P2.65 " " x_ang=" dx " y_ang=" dy " z_ang=" dz
  where
    d = vec 2 (-4) 1
    dx = radians2Degrees $ acos ((xComp d) / (magnitude d))
    dy = radians2Degrees $ acos ((yComp d) / (magnitude d))
    dz = radians2Degrees $ acos ((zComp d) / (magnitude d))


p67 :: String
p67 = prettyPrint "P2.67 " " a=" (a >< c) "\n       b=" (a >< f) "\n       c=" (d >< c)
                    "\n       d=" (a >< (f ^+^ (2 *^ c))) "\n       e=" (iHat >< b)
                    "\n       f=" (jHat >< b)
                    "\n       g=" ((vec 3 (-1) 0) >< b)
                    "\n       h=" (b><b)
  where
    a = vec2D 10 30
    b = vec2D 5 53
    c = vec2D 12 300
    d = vec2D 20 (180-37)
    f = vec2D 20 210


p69 :: String
p69 = prettyPrint "P2.69 " " a=" ans_a "\n       b=" ans_b "\n       c=" ans_c
  where
    a = vec2D 10 30
    b = vec2D 5 53
    d = vec2D 20 (180-37)
    f = vec2D 20 210
    ans_a = (a >< f) <.> d
    ans_b = (a >< f) <.> (d >< b)
    ans_c = (a <.> f) *^ (d >< b)
