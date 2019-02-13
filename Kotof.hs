module Kotof
(succ',even',odd'
) where

succ' :: Enum a => a -> (a -> a) -> a
succ' x f = f (succ x) 

even' :: Integral a => a -> (Bool -> Bool) -> Bool
even' x f = f (even x)

odd' :: Integral a => a -> (Bool -> Bool) -> Bool
odd' x f = f (odd x)
