import Data.Ratio
import Data.List (isSuffixOf)
import Kotof

type Function = [Char] 
data Math = Math Function String Int Int

main :: IO ()
main = do
    putStrLn "***begin your kotoha***"
    loop

loop :: IO ()
loop = do
    l <- getLine
    let ls = words l
    if l=="exit" then return () 
                 else do
                    let result = show (foldl evaluate 0 ls)
                    putStrLn (display result)
                    loop

display :: String -> String
display st
  | isSuffixOf "% 1" st = take (length st - 4) st
  | otherwise = map (\x -> if x=='%' then '/' else x) st 

evaluate :: Ratio Int -> String -> Ratio Int 
evaluate ac st = ac + section (Math [] [] 1 1) st

section :: Math -> String -> Ratio Int 
section (Math "" st n d) [] = (read st::Int) % d 
section (Math "*" st n d) [] = (read st::Int)*n % d
section (Math "/" st n d) [] = n % ((read st::Int)*d)
section (Math "" st n d) (x:xs)
  | x>='0' && x<='9' || x=='-' = section (Math [] (st++[x]) n d) xs
  | x=='x' || x=='*' = section (Math "*" [] (read st::Int) d) xs 
  | x=='/' = section (Math "/" [] (read st::Int) d) xs 
  | otherwise = section (Math [] st n d) xs
section (Math fs st n d) (x:xs)
  | (x>='0' && x<='9') || x=='-' = section (Math fs (st++[x]) n d) xs
  | fs=="*" && (x=='x' || x=='*') = section (Math "*" [] ((read st::Int)*n) d) xs 
  | fs=="*" && x=='/' = section (Math "/" [] ((read st::Int)*n) d) xs 
  | fs=="/" && (x=='x' || x=='*') = section (Math "*" [] n ((read st::Int)*d)) xs 
  | fs=="/" && x=='/' = section (Math "/" [] n ((read st::Int)*d)) xs 
  | otherwise = section (Math fs st n d) xs
