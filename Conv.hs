import System.Environment (getArgs)
import System.IO (IOMode(..), openFile, hClose, hSetEncoding, utf8, hPutStr)

conv :: [String] -> String
conv (x:[]) = (inPar 0 $ words x) ++ " $ " ++ initParam x
conv (x:y:[]) = (inPar 1 $ words x) ++" "++ (inPar 3 $ words y) ++ " $ " ++ initParam x
conv (x:xs) = (inPar 1 $ words x)
              ++" "++ (unwords $ map (inPar 2) $ init (map words xs))
              ++" "++ (inPar 3 $ words (last xs))
              ++ " $ " ++ initParam x

inPar :: Int -> [String] -> String
inPar i (x:[]) = change i x
inPar i ls = "("++ (unwords (map chInPar (init ls))) ++" "++ (chInPar (last ls)) ++")"++(lastPar i)

initParam :: String -> String
initParam s = case (foldl whatis "NULL" s) of
                "TUPPLE" -> s
                _ -> "0"

change :: Int -> String -> String
change i s = case (foldl whatis "NULL" s) of
             "NULL" -> ""
             "PERIOD" -> if (i==1 || i==2) then (init s)++" .>"
                                           else init s
             "NUM" -> if (i==1 || i==2) then "(+("++(replace s)++")) .>"
                                        else "(+("++(replace s)++"))"
             "NFUNC" -> if (i==1 || i==2) then "("++[(head (replace s))]++"("++(tail (replace s))++")) .>"
                                          else "("++[(head (replace s))]++"("++(tail (replace s))++"))"
             "LIST" -> case i of
                          0 -> "toList .> (++ "++(repStr s)++" )"
                          1 -> "toList .> (++ "++(repStr s)++" ) .>"
                          2 -> "(++ "++(repStr s)++") .> "
                          3 -> "(++ "++(repStr s)++")"
             "TUPPLE" -> case i of
                           0 -> "id"
                           1 -> ""
                           2 -> s++" .>"
                           3 -> s
             "FUNC" -> if (i==1 || i==2) then s++" .>"
                                         else s
             _ -> s 

chInPar :: String -> String
chInPar s = case (foldl whatis "NULL" s) of
             "NULL" -> ""
             "PERIOD" -> init s
             "NUM" -> replace s
             "NFUNC" -> "("++[(head (replace s))]++"("++(tail (replace s))++"))"
             "LIST" -> repStr s
             "FUNC" -> s
             _ -> s 

lastPar :: Int -> String
lastPar i = if (i==1 || i==2) then " .>"
                              else ""
                              
whatis :: String -> Char -> String
whatis acc ch
  | (ch>='0' && ch<='9') && acc=="NULL" = "NUM"
  | (ch>='0' && ch<='9') && acc=="NUM" = "NUM"
  | (ch>='0' && ch<='9') && acc=="PERIOD" = "NUM"
  | (ch=='+' || ch=='*' || ch=='x') && acc=="NULL" = "NFUNC"
  | ch=='+' && acc=="NFUNC" = "FUNC"
  | ch=='[' && acc=="NULL" = "LIST"
  | ch=='"' && acc=="NULL" = "LIST"
  | ch=='(' && acc=="NULL" = "TUPPLE?"
  | ch==')' && acc=="NULL" = "FUNC"
  | ch==',' && acc=="TUPPLE?" = "TUPPLE"
  | ch==')' && acc=="TUPPLE?" = "FUNC"
  | ch=='-' && acc=="NULL" = "NUM"
  | ch=='x' && acc=="NFUNC" = "NFUNC"
  | ch=='$' || ch=='\\' || ch=='>' && acc=="NULL" = "FUNC"
  | (ch>='a' && ch<='z') && acc=="NULL" = "FUNC"
  | (ch>='a' && ch<='Z') && acc=="NFUNC" = "FUNC"
  | (ch=='+' || ch=='*' || ch=='x' || ch=='/') && acc=="NUM" = "NUM"
  | ch=='.' && acc=="NUM" = "PERIOD"
  | otherwise = acc 

replace :: String -> String
replace s = foldr check [] s
              where check ch acc
                      | ch=='x' = '*':acc
                      | ch=='/' && (head acc)=='%' = '/':(tail acc)
                      | ch=='/' = '%':acc
                      | otherwise = ch:acc

repStr :: String -> String
repStr s = foldr check [] s
              where check ch acc
                      | ch==':' && (head acc)=='\\' = ':':(tail acc)
                      | ch==':' = ' ':acc
                      | otherwise = ch:acc

numPar :: String -> Int
numPar = foldl (\acc x -> if x==')' then acc+1 else acc) 0

joinPar :: String -> Bool -> Int -> [String] -> [String]
joinPar "" _ _ [] = []
joinPar s _ _ [] = [s]
joinPar s False i (x:xs)
  | head x == '(' && last x == ')' = x : (joinPar "" False i xs)
  | head x == '(' = joinPar (tail x) True (i+1) xs
  | otherwise = x : (joinPar "" False i xs)
joinPar s True i (x:xs)
  | head x == '(' && last x == ')' 
        = if (i-(numPar x)+1)==0 then (s++" "++(init x)):(joinPar "" False 0 xs) 
                                 else joinPar (s++" "++x) True (i-(numPar x)+1) xs
  | head x == '(' = joinPar (s++" "++x) True (i+1) xs
  | last x == ')' 
        = if (i-(numPar x))==0 then (s++" "++(init x)):(joinPar "" False 0 xs) 
                               else joinPar (s++" "++x) True (i-(numPar x)) xs
  | otherwise = joinPar (s++" "++x) True i xs

joinLst :: String -> Bool -> [String] -> [String]
joinLst "" _ [] = []
joinLst s _ [] = [s]
joinLst s False (x:xs)
  | head x == '[' && last x == ']' = x : (joinLst "" False xs)
  | head x == '[' = joinLst x True xs
  | otherwise = x : (joinLst s False xs)
joinLst s True (x:xs)
  | last x == ']' = (s++":"++x) : (joinLst "" False xs)
  | otherwise = joinLst (s++":"++x) True xs

fillStr :: Bool -> String -> String
fillStr _ "" = ""
fillStr False (x:xs)
  | x=='\"' = x:(fillStr True xs)
  | otherwise = x:(fillStr False xs)
fillStr True (x:xs)
  | x=='\\' = if (head xs)=='\"' then (head xs):(fillStr True (tail xs))
                               else x:(fillStr True xs)
  | x==' ' = ':':(fillStr True xs)
  | x==':' = ":\\"++(fillStr True xs)
  | x=='\"' = x:(fillStr False xs)
  | otherwise = x:(fillStr True xs)

checkLet :: String -> String -> (String, String)
checkLet s [] = ([],s)
checkLet s (x:[]) = checkLet (s++[x]) []
checkLet s (x:y:[]) = checkLet (s++[x]) [y]
checkLet s (x:y:xs) 
  = if (x==' ' && y=='=' && (head xs)==' ') then (s, (convAll (tail xs)))
                                            else (checkLet (s++[x]) (y:xs))

removeDollar :: String -> String
removeDollar [] = []
removeDollar s = if (last s)=='$' then (init s)
                                  else (removeDollar (init s))

convAll :: String -> String
convAll exp = conv (joinPar "" False 0 (joinLst "" False (words $ fillStr False exp)))

writeToFile :: String -> String -> IO ()
writeToFile fn s = do
  hout <- openFile fn AppendMode 
  hSetEncoding hout utf8
  let result = s++"\n\n"
  hPutStr hout result 
  hClose hout

main :: IO ()
main = do
  args <- getArgs
  let exp = head args
  let cl = checkLet "" exp
  if (fst cl)=="" then print (convAll exp) 
                  else do
                      writeToFile "User.hs" ((fst cl)++" = "++(removeDollar (snd cl)))
                      print (snd cl)

