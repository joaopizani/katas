module Evolution ( evolve
                 , Board ) where

import Data.List (replicate)


type Board = [[Bool]]


evolve :: Board -> Board
evolve b = dummyLine : newKernel ++ [dummyLine]
    where
        (ls,cs) = (length b, length $ head b)
        (innerLs,innerCs) = ([1 .. ls-2], [1 .. cs-2])
        dummyLine = replicate cs False
        dummyCel  = False
        newKernel = [dummyCel : [eval b l c | c <- innerCs] ++ [dummyCel]  |  l <- innerLs]


eval :: Board -> Int -> Int -> Bool
eval b l c
    | n < 2 || n > 3  = False
    | n == 3          = True
    | otherwise       = b !! l !! c
        where n = aliveNeighbours b l c


aliveNeighbours :: Board -> Int -> Int -> Int
aliveNeighbours b l c = length $ filter id [nw, n, ne, w, e, sw, s, se]
    where
        (nw,  n,  ne) = ( b !! (l-1) !! (c-1),  b !! (l-1) !! c,  b !! (l-1) !! (c+1) )
        (w,        e) = ( b !! l     !! (c-1),                    b !! l     !! (c+1) )
        (sw,  s,  se) = ( b !! (l+1) !! (c-1),  b !! (l+1) !! c,  b !! (l+1) !! (c+1) )
