count :: Integer -> Int

count n
  | n==0
    = 1
  | n > 0
    = (length (digs n)) -1
  |n < 0
    = (length (digs (negate n)))-1
  |otherwise
    = 1


digs :: Integral x => x -> [x]
digs 0 = [0]
digs x 
  | ((x `mod` 10 == 0) || (x `mod` 10 == 3) || (x `mod` 10 == 6) || (x `mod` 10 == 9))
    = x `mod` 10 : digs (x `div` 10)
  | (x `div` 10 == 0)
    = [x]  
  | otherwise
    = digs (x `div` 10)
