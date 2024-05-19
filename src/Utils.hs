{-# LANGUAGE FlexibleInstances #-}

module Utils (prettyPrint) where

import Text.Printf
import Vectors


class PP r where
    pp :: String -> r

instance PP String where
    pp = id

instance (PP r) => PP (Char -> r) where
    pp s c = pp (s ++ [c])

instance (PP r) => PP (Integer -> r) where
    pp s c = pp (s ++ (show c))

instance (PP r) => PP (Double -> r) where
    pp s c = pp (s ++ (printf "%+.3e" c))

instance (PP r) => PP (Vec -> r) where
    pp s c = pp (s ++ (show c))

instance (PP r) => PP (String -> r) where
    pp s c = pp (s ++ c)


prettyPrint :: (PP r) => r
prettyPrint = pp ""

