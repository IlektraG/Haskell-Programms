sumfab :: (Int->Int->Int->Int)->Int->Int->Int

sumfab f a b = sumXYZ f a a b b


sumXYZ :: (Int->Int->Int->Int)->Int->Int->Int->Int->Int
sumXYZ f a k b v
 = if k == b
    then f a k v
    else (sumXYZ f a k m v)
       + (sumXYZ f a (m+1) b v)
  where m = (k + b) `div` 2

giveB :: Int -> Int->Int
giveB b k = k*b
