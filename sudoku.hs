module Sudoku where

import Data.List as List
import Control.Monad
import Data.Maybe

type Value = Int
type Row = [[Value]]
--data Row = [[Value]] deriving (Eq,Ord)
type Board = [Row]

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
            
diffBoard = [[[],[3],[5],[],[],[],[4],[],[2]]
            ,[[],[],[],[],[],[],[],[6],[]]
            ,[[],[2],[],[],[6],[],[9],[],[]]
            ,[[],[],[],[],[],[6],[],[4],[]]
            ,[[9],[],[7],[4],[5],[],[],[1],[]]
            ,[[],[],[],[],[],[],[5],[],[9]]
            ,[[],[],[],[],[],[],[],[],[]]
            ,[[],[4],[1],[],[],[2],[3],[],[]]
            ,[[],[],[3],[],[],[7],[],[2],[4]]]::Board


hardBoard =   [[[2],[1],[],[],[],[],[4],[],[]]
            ,[[],[],[],[],[2],[8],[],[],[]]
            ,[[],[],[],[],[],[],[1],[],[6]]
            ,[[],[],[],[5],[],[7],[6],[],[8]]
            ,[[8],[3],[],[],[],[],[],[],[7]]
            ,[[],[],[],[],[1],[6],[],[],[3]]
            ,[[],[4],[2],[3],[],[],[],[],[]]
            ,[[],[5],[3],[],[],[],[],[7],[]]
            ,[[],[],[7],[9],[],[],[],[],[]]]::Board          
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

onePass xs = foldr (\x y -> onePass' (remove x) y) xs [1,2,3,4] 
    where onePass' f xs = box' . f . box' . cols . f . cols $ f xs

solved xs = rowsIdent xs && (rowsIdent . cols) xs && (rowsIdent . box') xs
    where rowsIdent = all (\x -> sort x == [[1],[2],[3],[4],[5],[6],[7],[8],[9]])
valid :: Board -> Bool
valid xs = and (map and $ map (\x -> map (not . null) x) xs) && not (or (map dupeSingle xs) || or (map (dupeSingle.box') xs) || or (map (dupeSingle.cols) xs))
dupeSingle xs = let single = filter (\x -> length x == 1) xs
    in (any (>1) $ map (\x -> (length . filter  (== x)) single) single)
reduce xs = let interim = onePass xs
            in if interim == xs
            then xs
            else reduce interim


testRow = [[1::Int],[3],[4,5,6],[7],[9],[2],[8],[4,5],[5,6]]
test = (allGuesses . reduce . build) testBoard
allGuesses :: Board -> [Board]
allGuesses ys = fromMaybe [] $ fmap (\y -> map (\z -> unConcat z) y) $
        liftM3 guesses' (fmap (\x -> take x xs) guessLoc) guesses (fmap (\x -> drop (x + 1) xs) guessLoc)
    where guesses' xs ys zs = map (\z -> xs ++ [[z]] ++zs) ys
          guessLoc = snd $ guess xs
          --(hd, tl) = splitAt (snd guesses) ys
          guesses = fst $ guess xs
          guess xs = (find (\x -> length x > 1) xs, findIndex(\x -> length x > 1) xs)
          xs = concat ys
solveAll xs = let xss = reduce xs
                in if solved xss
                    then [xss]
                    else let y = (filter valid $ fmap reduce $ allGuesses xss ) 
                            in concat $ fmap solveAll y
display xs = do
    putStrLn $ (rowToString . head) xs 
    if not.null $ tail xs 
        then display $ tail xs
        else putStrLn ""
        
rowToString :: Row -> String
rowToString xs 
    | length xs == 1 = cellToString $ head xs 
    | otherwise      = (cellToString $ head xs) ++ " " ++ (rowToString $ tail xs)
cellToString :: [Value] -> String
cellToString xs 
    | null xs        = " "
    | length xs == 1 = show $ head xs 
    | otherwise      = (show $ head xs) ++ (cellToString $ tail xs)
       