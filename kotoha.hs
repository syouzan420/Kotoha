import Data.Ratio
import Data.List (isSuffixOf)

type Function = Char; type Stack = String
type Numerator = Int; type Denominator = Int; type Flag = Bool
data Math = Math [Function] Stack Numerator Denominator Flag  

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
       let result = show (foldl selection 0 ls)
       putStrLn (display result)
       loop

selection :: Ratio Int -> String -> Ratio Int
selection a s@(x:xs)
  | (x>='0' && x<='9')||x=='-'||x=='+'||x=='(' =  a + mathToRatio (foldl calculate (Math "" "" 1 1 True) s)
  | s=="succ" = ((numerator a)+1) % (denominator a)
  | s=="pred" = ((numerator a)-1) % (denominator a)
  | otherwise = 0 % 1

display :: String -> String
display st
  | isSuffixOf "% 1" st = take (length st - 4) st
  | otherwise = map (\x -> if x=='%' then '/' else x) st 

mathToRatio :: Math -> Ratio Int
mathToRatio (Math [] st n d _) = (read st) % d
mathToRatio (Math [')'] _ n d _) = n % d
mathToRatio (Math ['*'] st n d b) = if b then (((read st)*n) % d) else (n % ((read st)*d))
mathToRatio (Math ['x'] st n d b) = if b then (((read st)*n) % d) else (n % ((read st)*d))
mathToRatio (Math ['/'] st n d b) = if b then (n % ((read st)*d)) else (((read st)*n) % d)
mathToRatio (Math ['^'] st n d b) = if b then ((n^(read st)) % d) else (n % (d^(read st)))

calculate :: Math -> Char -> Math
calculate (Math [] st n d _) x
  | x>='0' && x<='9' || x=='-' = Math [] (st++[x]) n d True
  | x=='x' || x=='*' || x=='/' || x=='^' = Math [x] "" (read st) d True
  | otherwise = Math [] st n d True
calculate (Math fl@(f:fs) st n d b) x
  | (x>='0' && x<='9') || x=='-' = Math fl (st++[x]) n d b
  | f=='/' && x=='(' = Math (x:fl) st n d (not b)
  | x==')' && length fl>1 && (((f=='x' || f=='*') && b) || (f=='/' && not b))
            = Math (x:tail fs) "" ((read st)*n) d (not b) 
  | x==')' && length fl>1 && (((f=='x' || f=='*') && not b) || (f=='/' && b))
            = Math (x:tail fs) "" n ((read st)*d) (not b) 
  | x==')' && (((f=='x' || f=='*') && b) || (f=='/' && not b)) = Math [x] "" ((read st)*n) d b 
  | x==')' && (((f=='x' || f=='*') && not b) || (f=='/' && b)) = Math [x] "" n ((read st)*d) b 
  | f==')' && (x=='x' || x=='*' || x=='/' || x=='^') = Math [x] "" n d b 
  | (((f=='x' || f=='*' || f=='(') && b)||(f=='/' && not b)) && (x=='x' || x=='*' || x=='/' || x=='^')
            = Math (x:fs) "" ((read st)*n) d b 
  | (((f=='x' || f=='*' || f=='(') && not b)||(f=='/' && b)) && (x=='x' || x=='*' || x=='/' || x=='^')
            = Math (x:fs) "" n ((read st)*d) b 
  | f=='^' && b && (x=='x' || x=='*' || x=='/' || x=='^') = Math (x:fs) "" (n^(read st)) d b
  | f=='^' && not b && (x=='x' || x=='*' || x=='/' || x=='^') = Math (x:fs) "" n (d^(read st)) b
  | otherwise = Math fl st n d b
