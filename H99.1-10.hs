
myLast :: [a] -> a
myLast []     = error "Empty lists have no last element"
myLast [x]    = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast []   = error "Empty lists have no last but one element"
myButLast [x]   = error "Single lists have no last but one element"
myButLast (x:y:ys)
          | null ys   = x
          | otherwise = myButLast (y:ys)

elementAt :: [a] -> Int -> a
elementAt [] k = error "Empty lists have no k element"
elementAt (x:xs) k
          | k < 0             = error "k must be a positive integer"
          | null xs && k > 1  = error "k exceedes the length of the list"
          | k == 1            = x
          | otherwise         = elementAt xs (k-1)