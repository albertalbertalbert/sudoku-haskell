module Sudoku where

import Data.List as List
import Control.Monad
import Data.Maybe

type Value = Int
type Cell = [Value] 
type Board = [[Cell]]

a = [[1::Value,2,3,4,5,6,7,8,9],[9,8,7,6,5,4,3,2,1],[2,3,4,5,6,7,8,9,1],[3,4,5,6,7,8,9,1,2],[4,5,6,7,8,9,1,2,3],[5,6,7,8,9,1,2,3,4],
    [6,7,8,9,1,2,3,4,5],[7,8,9,1,2,3,4,5,6],[8,9,1,2,3,4,5,6,7]]
cols = List.transpose 
unConcat xs =   let (x,y) = splitAt 9 xs
                in if length y <= 9
                then (x:[y])
                else (x:unConcat y)

testBoard :: Board
testBoard = [[[3::Value],[2],[],[],[],[1],[],[],[]]
            ,[[],[],[1],[7],[],[],[8],[2],[]]
            ,[[],[],[],[],[],[3],[1],[9],[5]]
            ,[[9],[8],[],[],[],[],[],[],[]]
            ,[[],[5],[2],[],[],[],[3],[6],[]]
            ,[[],[],[],[],[],[],[],[8],[1]]
            ,[[4],[3],[7],[5],[],[],[],[],[]]
            ,[[],[6],[5],[],[],[4],[9],[],[]]
            ,[[],[],[],[3],[],[],[],[4],[8]]]
            
valid_test1 = [[]]
valid_test2 = [[1,2],[3]]
valid_test3 = []
    
box' xs = map (boxRow xs) [0,3,6] 
    ++ (map (boxRow ((take 3 . drop 3) xs)) [0,3,6])
    ++ (map (boxRow ((take 3 . drop 6) xs)) [0,3,6])
    where boxRow xs n = concat $ take 3 $ map (take 3 . drop n) xs
build xs = map (map (\x -> if x == []
                        then [1,2,3,4,5,6,7,8,9]
                        else x)) xs
                        
remove n ys = map (\xs -> foldr remove' xs [x|x<-xs, length x == n, ( length.filter (==x)) xs >= n]) ys
    where remove' x y = map(\z -> if x == z then x else z List.\\ x) y

onePass xs = foldr (\x y -> onePass' (remove x) y) xs [1]--,2,3,4] 
    where onePass' f xs = box' . f . box' . cols . f . cols $ f xs

solved xs = rowsIdent xs && (rowsIdent . cols) xs && (rowsIdent . box') xs
    where rowsIdent = all (\x -> sort x == [[1],[2],[3],[4],[5],[6],[7],[8],[9]])

valid xs = and (map (not . null) xs) && not (or (map dupeSingle xs) || or (map (dupeSingle.box') xs) || or (map (dupeSingle.cols) xs))
dupeSingle xs = let single = filter (\x -> length x == 1) xs
    in (any (>1) $ map (\x -> (length . filter  (== x)) single) single)
reduce xs = let interim = onePass xs
            in if interim == xs
            then xs
            else reduce interim


testRow = [[1::Int],[3],[4,5,6],[7],[9],[2],[8],[4,5],[5,6]]
test = (allGuesses . reduce . build) testBoard

allGuesses ys = fmap (\y -> map (\z -> unConcat z) y) $
        liftM3 guesses' (fmap (\x -> take x xs) guessLoc) guesses (fmap (\x -> drop (x + 1) xs) guessLoc)
    where guesses' xs ys zs = map (\z -> xs ++ [[z]] ++zs) ys
          guessLoc = snd $ guess xs
          guesses = fst $ guess xs
          guess xs = (find (\x -> length x > 1) xs, findIndex(\x -> length x > 1) xs)
          xs = concat ys

display xs = do
    putStrLn $ (show . head) xs
    putStrLn $ (show . head . tail) xs
    putStrLn $ (show . head. tail .tail ) xs
    putStrLn $ (show . head . tail . tail . tail ) xs
    putStrLn $ (show . head . tail . tail . tail . tail ) xs
    putStrLn $ (show . head . tail . tail . tail . tail . tail) xs
    putStrLn $ (show . head . tail . tail . tail . tail . tail . tail) xs
    putStrLn $ (show . head . tail . tail . tail . tail . tail . tail . tail) xs
    putStrLn $ (show . head . tail . tail . tail . tail . tail . tail . tail . tail) xs
    
    
    
--test2 = fmap (\y -> map (\z -> unConcat z) y) test        