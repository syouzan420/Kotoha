import System.Environment (getArgs)

conv :: [String] -> String
conv (x:[]) = (inPar 0 (words x)) ++ " $ " ++ initParam x
conv (x:y:[]) = (inPar 1 (words x)) ++ (inPar 3 (words y)) ++ " $ " ++ initParam x
conv (x:xs) = (inPar 1 (words x))
              ++ (unwords $ map (inPar 2) (init (map words xs)))
              ++ (inPar 3 (words (last xs)))
              ++ " $ " ++ initParam x

inPar :: Int -> [String] -> String
inPar i (x:[]) = change i x
inPar i ls = (foldl ((++).(change 0)) "" (init ls)) ++" "++ (change i (last ls))

change :: Int -> String -> String
change i s = case (foldl whatis "NULL" s) of
             "NULL" -> ""
             "PERIOD" -> if (i==1 || i==2) then (init s)++" .> "
                                           else init s
             "NUM" -> if (i==1 || i==2) then "(+("++(replace s)++")) .> "
                                        else "(+("++(replace s)++"))"
             "NFUNC" -> if (i==1 || i==2) then "("++[(head (replace s))]++"("++(tail (replace s))++")) .> "
                                          else "("++[(head (replace s))]++"("++(tail (replace s))++"))"
             "LIST" -> case i of
                          0 -> "toList .> (++ "++s++" ) .> tail"
                          1 -> "toList .> (++ "++s++" ).> tail .> "
                          2 -> s++" .>"
                          3 -> s 
             "FUNC" -> if (i==1 || i==2) then s++" .> "
                                         else s++" "
             _ -> ""

joinPar :: String -> Bool -> [String] -> [String]
joinPar _ _ [] = []
joinPar s False (x:xs)
  | head x == '(' = joinPar (tail x) True xs
  | otherwise = x : (joinPar s False xs)
joinPar s True (x:xs)
  | last x == ')' = (s++" "++(init x)) : (joinPar "" False xs)
  | otherwise = joinPar (s++" "++x) True xs

replace :: String -> String
replace s = foldr check [] s
              where check ch acc
                      | ch=='x' = '*':acc
                      | ch=='/' && (head acc)=='%' = '/':(tail acc)
                      | ch=='/' = '%':acc
                      | otherwise = ch:acc
 
whatis :: String -> Char -> String
whatis acc ch
  | (ch>='0' && ch<='9') && acc=="NULL" = "NUM"
  | (ch>='0' && ch<='9') && acc=="NUM" = "NUM"
  | (ch>='0' && ch<='9') && acc=="PERIOD" = "NUM"
  | (ch=='+' || ch=='*') && acc=="NULL" = "NFUNC"
  | ch=='[' && acc=="NULL" = "LIST_"
  | ch==']' && acc=="LIST_" = "LIST"
  | ch=='-' && acc=="NULL" = "NUM"
  | (ch>='a' && ch<='x') && acc=="NULL" = "FUNC"
  | (ch=='+' || ch=='*' || ch=='x' || ch=='/') && acc=="NUM" = "NUM"
  | ch=='.' && acc=="NUM" = "PERIOD"
  | otherwise = acc 

initParam :: String -> String
initParam _ = "0"

main :: IO ()
main = do
  args <- getArgs
  let exp = head args
  print (conv (joinPar "" False (words exp)))

