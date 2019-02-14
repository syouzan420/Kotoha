import Data.Ratio
import Data.List (isSuffixOf)
import Kotof

type Function = String; type Stack = String
type Numerator = Int; type Denominator = Int
data Math = Math Function Stack Numerator Denominator 

main :: IO ()
main = do
    putStrLn "***begin your kotoha***"
    loop

loop :: IO ()
loop = do
    l <- getLine
    let ls = words l
    if l=="exit"
      then return () 
      else do
         let result = show (foldl (\a s -> a+(mathToRatio (foldl calculate (Math "" "" 1 1) s))) 0 ls)
         putStrLn (display result)
         loop

display :: String -> String
display st
  | isSuffixOf "% 1" st = take (length st - 4) st
  | otherwise = map (\x -> if x=='%' then '/' else x) st 

mathToRatio :: Math -> Ratio Int
mathToRatio (Math "" st n d) = (read st) % d
mathToRatio (Math "*" st n d) = (read st)*n % d
mathToRatio (Math "/" st n d) = n % ((read st)*d)

calculate :: Math -> Char -> Math
calculate (Math "" st n d) x
  | x>='0' && x<='9' || x=='-' = Math "" (st++[x]) n d
  | x=='x' || x=='*' = Math "*" "" (read st) d 
  | x=='/' = Math "/" "" (read st) d 
  | otherwise = Math "" st n d
calculate (Math fs st n d) x
  | (x>='0' && x<='9') || x=='-' = Math fs (st++[x]) n d
  | fs=="*" && (x=='x' || x=='*') = Math "*" "" ((read st)*n) d 
  | fs=="*" && x=='/' = Math "/" "" ((read st)*n) d 
  | fs=="/" && (x=='x' || x=='*') = Math "*" "" n ((read st)*d) 
  | fs=="/" && x=='/' = Math "/" "" n ((read st)*d) 
  | otherwise = Math fs st n d
