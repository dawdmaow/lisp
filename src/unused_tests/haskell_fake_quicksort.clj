;; Haskell's famous "quicksort" algorithm, just for fun.
(quicksort := (\ (a) any
    (if (empty? a) (return a))
    (middle := (array-get a 0))
    (lesser := (filter-array a (\ ((it any)) any (it < middle))))
    (greater := (filter-array a (\ ((it any)) any (it >= middle))))
    (repr lesser)
    (repr greater)
    (join-lists (quicksort lesser) (quote (middle)) (quicksort greater))
)) 

(data := (quote (3 6 8 10 1 2 1)))
(sorted := (quicksort data))
(sorted must-equal (quote (1 1 2 3 6 8 10)))