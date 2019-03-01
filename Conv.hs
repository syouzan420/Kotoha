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
inPar i ls = "("++ (unwords (map chInPar (init ls))) ++" "++ (chInPar (last ls)) ++")"++(lastPar i)

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
                          0 -> "toList .> (++ "++(repStr s)++" )"
                          1 -> "toList .> (++ "++(repStr s)++" ) .> "
                          2 -> "(++ "++(repStr s)++") .> "
                          3 -> "(++ "++(repStr s)++")"
             "FUNC" -> if (i==1 || i==2) then s++" .> "
                                         else s++" "
             _ -> ""

chInPar :: String -> String
chInPar s = case (foldl whatis "NULL" s) of
             "NULL" -> ""
             "PERIOD" -> init s
             "NUM" -> replace s
             "NFUNC" -> "("++[(head (replace s))]++"("++(tail (replace s))++"))"
             "LIST" -> repStr s
             "FUNC" -> s++" "
             _ -> ""

lastPar :: Int -> String
lastPar i = if (i==1 || i==2) then " .> "
                              else ""


joinPar :: String -> Bool -> [String] -> [String]
joinPar _ _ [] = []
joinPar s False (x:xs)
  | head x == '(' = joinPar (tail x) True xs
  | otherwise = x : (joinPar s False xs)
joinPar s True (x:xs)
  | last x == ')' = (s++" "++(init x)) : (joinPar "" False xs)
  | otherwise = joinPar (s++" "++x) True xs

joinStr :: String -> Bool -> [String] -> [String]
joinStr "" _ [] = []
joinStr s _ [] = [s]
joinStr s False (x:xs)
  | head x == '"' = joinStr x True xs
  | otherwise = x : (joinStr s False xs)
joinStr s True (x:xs)
  | last x == '"' = (s++":"++x) : (joinStr "" False xs)
  | otherwise = joinStr (s++":"++x) True xs

replace :: String -> String
replace s = foldr check [] s
              where check ch acc
                      | ch=='x' = '*':acc
                      | ch=='/' && (head acc)=='%' = '/':(tail acc)
                      | ch=='/' = '%':acc
                      | ch==':' = ' ':acc
                      | otherwise = ch:acc

repStr :: String -> String
repStr s = foldr check [] s
              where check ch acc
                      | ch==':' && (head acc)==' ' = ':':acc
                      | ch==':' = ' ':acc
                      | otherwise = ch:acc
 
whatis :: String -> Char -> String
whatis acc ch
  | (ch>='0' && ch<='9') && acc=="NULL" = "NUM"
  | (ch>='0' && ch<='9') && acc=="NUM" = "NUM"
  | (ch>='0' && ch<='9') && acc=="PERIOD" = "NUM"
  | (ch=='+' || ch=='*' || ch=='x') && acc=="NULL" = "NFUNC"
  | ch=='+' && acc=="NFUNC" = "FUNC"
  | ch=='[' && acc=="NULL" = "LIST"
  | ch=='"' && acc=="NULL" = "LIST"
  | ch=='-' && acc=="NULL" = "NUM"
  | ch=='x' && acc=="NFUNC" = "NFUNC"
  | (ch>='a' && ch<='z') && acc=="NULL" = "FUNC"
  | (ch>='a' && ch<='Z') && acc=="NFUNC" = "FUNC"
  | (ch=='+' || ch=='*' || ch=='x' || ch=='/') && acc=="NUM" = "NUM"
  | ch=='.' && acc=="NUM" = "PERIOD"
  | otherwise = acc 

initParam :: String -> String
initParam _ = "0"

main :: IO ()
main = do
  args <- getArgs
  let exp = head args
  print (conv (joinPar "" False (joinStr "" False (words exp))))

