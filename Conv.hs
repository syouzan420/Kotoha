import System.Environment (getArgs)

conv :: String -> String
conv line = (unwords $ map change $ init $ words line)
              ++ (changeLast $ last $ words line)
              ++ " $ " ++ initParam (head (words line))

change :: String -> String
change s = case (foldl whatis "NULL" s) of
             "NULL" -> ""
             "NUM" -> "(+("++(replace s)++")) .> "
             "NFUNC" -> "("++(replace s)++") .> "
             "FUNC" -> "("++s++") .> "
             _ -> ""

changeLast :: String -> String
changeLast s = case (foldl whatis "NULL" s) of
                 "NULL" -> ""
                 "NUM" -> "(+("++(replace s)++"))"
                 "NFUNC" -> "("++(replace s)++")"
                 "FUNC" -> "("++s++")"
                 _ -> ""

replace :: String -> String
replace s = foldr check [] s
              where check ch acc
                      | ch=='x' = '*':acc
                      | ch=='/' = '%':acc
                      | otherwise = ch:acc
 
whatis :: String -> Char -> String
whatis acc ch
  | (ch>='0' && ch<='9') && (acc=="NULL" || acc=="NUM") = "NUM"
  | (ch=='+' || ch=='*') && (acc=="NULL") = "NFUNC"
  | ch=='-' && acc=="NULL" = "NUM"
  | (ch=='+' || ch=='*' || ch=='x' || ch=='/' || ch=='.') && (acc=="NUM") = "NUM"
  | otherwise = "FUNC"

initParam :: String -> String
initParam _ = "0"

main :: IO ()
main = do
  args <- getArgs
  let exp = head args
  print (conv exp)

