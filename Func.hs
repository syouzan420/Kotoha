module Func ((.>)) where

infixr 5 .>
(.>) :: Functor f => f a -> (a -> b) -> f b
(.>) = flip fmap

toList :: a -> [b]
toList _ = []

cons :: a -> [a] -> [a]
cons x xs = x:xs

index :: Int -> [a] -> a
index = flip (!!)

add :: Num a => a -> a -> a
add = (+)

times :: Num a => a -> a -> a
times = (*)

