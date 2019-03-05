module Func
((.>),
toList,
cons,
index,
add,
times) where

import Control.Monad(liftM)

--infixr 5 .>
--(.>) :: Functor f => f a -> (a -> b) -> f b
--(.>) = flip fmap 

infixr 5 .>
(.>) :: Monad m => m a -> (a -> b) -> m b
(.>) = flip liftM 

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

