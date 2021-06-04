nearest :: [Int]->Int->Int
nearest s n
  | s==[]
    = error "error : empty list!"
  | otherwise  
    = findDistance s n (abs((s !! 0) - n)) 1 1


findDistance :: [Int] -> Int -> Int -> Int -> Int -> Int
findDistance (h:t) n minDistance el catchIt
  | t == [] && (h>=0 || h<0)
    = lastElem h n minDistance el catchIt
  |(abs(h-n) < minDistance)
    = findDistance t n (abs(h - n)) (el+1) el
  |(abs(h-n) >= minDistance)
    = findDistance t n minDistance (el+1) catchIt
  | otherwise
    = catchIt


lastElem :: Int->Int->Int->Int->Int->Int
lastElem h n minDistance el catchIt
  |(abs(h-n) < minDistance)
    = el
  |otherwise
    = catchIt
