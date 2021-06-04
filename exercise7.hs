swap :: String->String

swap s = listToString (changePos e 0 "" [] "")
  where e=split s ' '


listToString :: [String] -> String
listToString [] = ""
listToString (h:t)
  |length (h:t)==1
    = h ++ listToString t
  |otherwise
    = h ++ " " ++ listToString t


split :: String -> Char -> [String]
split [] delim = [""]
split (h:t) delim
    | h == delim = "" : rest
    | otherwise = (h : head rest) : tail rest
    where
        rest = split t delim


changePos :: [String]->Int->String->[String]->String->[String]
changePos q w prevv newCS nextNext
  | w==0
    = changePos q (w+1) (q !! 0) newCS (q !! 1) 
  | w+1<=(length q) && w `mod` 2 /=0
    = changePos q (w+1) (q !! (w-1)) (newCS ++ [nextNext]) (q !! (w+1))
    | w+1<=(length q) && w `mod` 2 ==0 && (w+2)<=length q
    = changePos q (w+1) (q !! (w-1)) (newCS ++ [prevv]) (q !! (w+1))
  | w+1<=(length q) && w `mod` 2 ==0 && (w+2)>length q
    = changePos q (w+1) (q !! (w-1)) (newCS ++ [prevv]) (q !! (w+1))
  | w+1>0 && (length q) `mod` 2 == 0
    = newCS ++ [prevv]
  |otherwise
    = newCS ++ [last q]
