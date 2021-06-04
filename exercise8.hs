mapi :: [u]->(u->Int->v)->[v]

mapi s f = findQ [] i s f
  where i=0
        q = []


findQ :: [v]->Int->[u]->(u->Int->v)->[v]
findQ q i s f
  | i<length s
    = findQ (q ++ [(f (s !! i) (i+1))]) (i+1) s f
  | otherwise
    = q
