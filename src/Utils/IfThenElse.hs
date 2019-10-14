module Utils.IfThenElse where

-- This is to get around a weird issue when using RebindableSyntax
ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y
  | b = x
  | otherwise = y
