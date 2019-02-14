import Data.Ratio
import Data.List (isSuffixOf)
import Kotof

type Function = String
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
evaluate ac st = ac + section [] 1 1 [] st

section :: String -> Int -> Int -> String -> String -> Ratio Int 
section st n d [] [] = (read st::Int) % d 
section st n d "*" [] = (read st::Int)*n % d
section st n d "/" [] = n % ((read st::Int)*d)
section st n d [] (x:xs)
  | x>='0' && x<='9' || x=='-' = section (st ++ [x]) n d [] xs
  | x=='x' || x=='*' = section [] (read st::Int) d "*" xs 
  | x=='/' = section [] (read st::Int) d "/" xs 
  | otherwise = section st n d [] xs
section st n d fs (x:xs)
  | (x>='0' && x<='9') || x=='-' = section (st ++ [x]) n d fs xs
  | fs=="*" && (x=='x' || x=='*') = section [] ((read st::Int)*n) d "*" xs 
  | fs=="*" && x=='/' = section [] ((read st::Int)*n) d "/" xs 
  | fs=="/" && (x=='x' || x=='*') = section [] n ((read st::Int)*d) "*" xs 
  | fs=="/" && x=='/' = section [] n ((read st::Int)*d) "/" xs 
  | otherwise = section st n d fs xs
