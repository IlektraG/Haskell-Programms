kgcd :: Int -> Int -> Int -> Int

kgcd m n k
  | m==n
    = findK k (reverse((deleteInt n m [0..n])))
  | m>n
    = findK k (reverse((deleteInt n m [0..n])))
  | m<n
    = findK k (reverse((deleteInt n m [0..m])))
  |otherwise
    =0


deleteInt :: Int -> Int -> [Int] -> [Int]
deleteInt n m (h:t)
  | h==0
    = h : deleteInt n m t
  | (n `mod` h == 0) && (m `mod` h == 0)
    = h : deleteInt n m t
  | otherwise 
    = deleteInt n m t
deleteInt n m []=[]


findK :: Int->[Int]->Int
findK k (h:t)
  | k==1
    = h
  | k>length (h:t)
    =0
  |otherwise
    = findK (k-1) t
