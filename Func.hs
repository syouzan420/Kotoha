module Func ((.>)) where

import Data.List (isSuffixOf)

infixr 5 .>
(.>) :: Functor f => f a -> (a -> b) -> f b
(.>) = flip fmap

display :: String -> String
display s
  | isSuffixOf "% 1" s = take (length s - 4) s
  | otherwise = map (\x -> if x=='%' then '/' else x) s 
