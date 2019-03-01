module Func ((.>)) where

infixr 5 .>
(.>) :: Functor f => f a -> (a -> b) -> f b
(.>) = flip fmap

toList :: a -> [b]
toList _ = []

