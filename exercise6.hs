smooth :: [Int] -> Int -> [Int]

smooth s k 
  | k==1
    = s
  | otherwise
    = teliko s k 1 0 []


teliko :: [Int]->Int->Int->Int->[Int]->[Int]
teliko s kStable k summ telikoss
  |k==1
    = teliko s kStable (k+1) (sumS s kStable 1 0 0 []) telikoss
  | (length telikoss)<((length s)-(kStable-1))
    = teliko s kStable (k+1) (sumS s kStable k 0 0 []) (telikoss ++ [summ `div` kStable])
  |otherwise
    = telikoss


sumS :: [Int] -> Int -> Int -> Int -> Int -> [Int] -> Int
sumS s kStable k  counting summarize pinakaki
  | counting+1<=kStable
    = sumS s kStable k (counting+1) summarize (pinakaki ++ [s !! ((counting-1)+k)])
  | otherwise
    =  sum pinakaki
