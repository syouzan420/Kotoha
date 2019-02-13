import Data.Ratio
import Data.List (isSuffixOf)
import Kotof

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
evaluate ac st = ac + section [] [] 1 [] st

section :: String -> String -> Int -> String -> String -> Ratio Int 
section st1 st2 i [] [] = (read st1::Int) % i
section st1 st2 i "*" [] = (read st1::Int)*(read st2::Int) % i
section st1 st2 i "/" [] = (read st1::Int)%((read st2::Int)*i)
section st1 st2 i [] (x:xs)
  | x>='0' && x<='9' || x=='-' = section (st1 ++ [x]) st2 i [] xs
  | x=='x' || x=='*' = section st1 st2 i "*" xs 
  | x=='/' = section st1 st2 i "/" xs 
  | otherwise = section st1 st2 i [] xs
section st1 st2 i fs (x:xs)
  | (x>='0' && x<='9') || x=='-' = section st1 (st2 ++ [x]) i fs xs
  | fs=="*" && (x=='x' || x=='*') = section (show ((read st1::Int)*(read st2::Int))) [] i "*" xs 
  | fs=="*" && x=='/' = section (show ((read st1::Int)*(read st2::Int))) [] i "/" xs 
  | fs=="/" && (x=='x' || x=='*') = section st1 [] ((read st2::Int)*i) "*" xs 
  | fs=="/" && x=='/' = section st1 [] ((read st2::Int)*i) "/" xs 
  | otherwise = section st1 st2 i fs xs
