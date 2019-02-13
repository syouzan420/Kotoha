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
evaluate ac st = ac + section [] [] [] st

section :: String -> String -> String -> String -> Ratio Int 
section st1 st2 [] [] = (read st1::Int)%1
section st1 st2 "*" [] = (read st1::Int)*(read st2::Int)%1
section st1 st2 "/" [] = (read st1::Int)%(read st2::Int)
section st1 st2 [] (x:xs)
  | x>='0' && x<='9' || x=='-' = section (st1 ++ [x]) st2 [] xs
  | x=='x' || x=='*' = section st1 st2 "*" xs 
  | x=='/' = section st1 st2 "/" xs 
  | otherwise = section st1 st2 [] xs
section st1 st2 fs (x:xs)
  | (x>='0' && x<='9') || x=='-' = section st1 (st2 ++ [x]) fs xs
  | fs=="*" && (x=='x' || x=='*') = section (show ((read st1::Int)*(read st2::Int))) [] "*" xs 
  | fs=="*" && x=='/' = section (show ((read st1::Int)*(read st2::Int))) [] "/" xs 
  | fs=="/" && (x=='x' || x=='*') = section (show ((read st1::Int)%(read st2::Int))) [] "*" xs 
  | fs=="/" && x=='/' = section (show ((read st1::Int)%(read st2::Int))) [] "/" xs 
  | otherwise = section st1 st2 fs xs
