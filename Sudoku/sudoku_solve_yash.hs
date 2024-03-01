-- Sudoku solver program by Yash Lele (u1470644)
-- How to run?
-- 1. Set appropriate dimensions m and n on lines 9 and 12
-- 2. Set the corresponding index for the example board to solve on line 217 (solves quickly for index 0,1,2,3)
----------------------------------
import Data.List (transpose)

m :: Int
m = 3

n :: Int
n = 3

type BldgBlock a = [a]
-- A BldgBlock can be a row, a column, or a sub-grid
type Board b = [BldgBlock b]

examples :: [Board Int]
examples = [[[1,0,3,  4,5,6], ------------------------- 3 x 2 : index 0
             [4,5,6,  0,2,3],
             --
             [2,3,4,  5,0,1],
             [5,6,0,  2,3,4],
             --
             [0,4,5,  6,1,2],
             [0,1,2,  3,0,5]],
            ------------------
            [[1,0,  0,4,  5,6],------------------------- 2 x 3 : index 1
             [0,4,  5,6,  1,0],
             [5,6,  1,0,  0,4],
             --
             [0,0,  4,5,  6,1],
             [4,5,  6,1,  0,0],
             [6,1,  0,0,  4,5]],
           ------------------------
           [[2,0,0,  0,0,1,  0,3,8],----------------------- 3 x 3 : index 2
           [0,0,0,  0,0,0,  0,0,5],
           [0,7,0,  0,0,6,  0,0,0],
           --
           [0,0,0,  0,0,0,  0,1,3],
           [0,9,8,  1,0,0,  2,5,7],
           [3,1,0,  0,0,0,  8,0,0],
           --
           [9,0,0,  8,1,0,  0,2,0],
           [0,5,0,  0,6,9,  7,8,4],
           [4,0,0,  2,5,0,  0,0,0]],
           ------------------------
          [[1,2,0,4,  5,6,7,0],---------------------------- 4 x 2 : index 3
           [5,6,7,0,  1,0,0,4],
           --
           [2,0,4,5,  6,7,8,0],
           [6,7,0,1,  0,3,4,5],
           --
           [3,4,5,0,  7,8,1,2],
           [7,8,1,2,  0,4,0,6],
           --
           [0,0,6,7,  8,1,2,3],
           [0,1,2,0,  4,0,6,7]],
           ------------------------
           [[0,0,0,  0,0,0,  0,1,5],
           [0,2,0,  0,0,0,  0,0,0],
           [0,0,0,  0,0,0,  4,0,8],
           --
           [0,0,3,  0,0,0,  9,0,0],
           [0,0,0,  1,0,0,  0,0,0],
           [0,0,0,  0,0,8,  0,0,0],
           --
           [1,0,0,  4,0,0,  0,0,0],
           [0,0,0,  0,7,0,  3,0,0],
           [8,0,0,  0,0,0,  0,6,0]],
           ------------------------
           [[9,0,6,  0,7,0,  4,0,3],
           [0,0,0,  4,0,0,  2,0,0],
           [0,7,0,  0,2,3,  0,1,0],
           --
           [5,0,0,  0,0,0,  1,0,0],
           [0,4,0,  2,0,8,  0,6,0],
           [0,0,3,  0,0,0,  0,0,5],
           --
           [0,3,0,  7,0,0,  0,5,0],
           [0,0,7,  0,0,5,  0,0,0],
           [4,0,5,  0,1,0,  7,0,8]]]
------------------------------------------------------
-- Logic to display the board nicely

stringifyBoard :: Show t => Board t -> String
stringifyBoard [] = ""
stringifyBoard rows = concatMap ((++ "\n") . visualize) (take n rows) ++ "\n" ++ stringifyBoard (drop n rows)

visualize :: Show t => BldgBlock t -> String
visualize [] = ""
visualize ints =  show (take m ints) ++ "  " ++ visualize (drop m ints)
------------------------------------------------------

------------------------------------------------------
-- Basic Utils

-- Given ints p,q , tell if the board is in correct p x q shape
isWellShaped :: Board t -> Int -> Int -> Bool
isWellShaped brd p q = (length brd == p*q) && all (\x -> length x == p*q) brd

rowsOf :: Board t -> [BldgBlock t]
rowsOf brd = brd

combineRows :: [BldgBlock t] -> Board t
combineRows = rowsOf

columnsOf :: Board t -> [BldgBlock t]
columnsOf = transpose

combineColumns :: [BldgBlock t] -> Board t
combineColumns = columnsOf

subgridsOf :: Board t -> [BldgBlock t]
subgridsOf [] = []
subgridsOf brd = getSubgridsInHorizontalDir (take n brd) ++ subgridsOf (drop n brd)
                 where getSubgridsInHorizontalDir brd@(x:_) =  if null x
                                                               then []
                                                               else concatMap (take m) brd : getSubgridsInHorizontalDir (map (drop m) brd)

combineSubgrids :: [BldgBlock t] -> Board t
combineSubgrids = subgridsOf

------------------------------------------------------

------------------------------------------------------
-- Logic to generate and narrow-down options

type MultiBoard t = Board [t]

createMxNoptions :: Board Int -> MultiBoard Int
createMxNoptions = map populateOptions
                   where populateOptions [] = []
                         populateOptions (x:xs) = (if x == 0 then [1..m*n] else [x]) : populateOptions xs

siftOptions :: Eq t => BldgBlock [t] -> BldgBlock [t]
siftOptions lstlstInt = let singles = concat (filter (\x -> length x == 1) lstlstInt)
                        in map (removeSingles singles) lstlstInt
                        where removeSingles sngls lstInt = if length lstInt == 1 then lstInt else filter (`notElem` sngls) lstInt

repeatUntilSame :: Eq t => (t->t) -> t -> t
repeatUntilSame fn arg = let fn_of_arg = fn arg
                         in if fn_of_arg == arg
                            then fn_of_arg
                            else repeatUntilSame fn fn_of_arg

keepSifting :: Eq t => [[t]] -> [[t]] -- MultiBoard t -> MultiBoard t
keepSifting = repeatUntilSame siftOptions

allFlatLists :: BldgBlock [t] -> [BldgBlock t]
-- This works like a Cartesian product of many sets
allFlatLists [] = [[]]
allFlatLists (list : lists) = [eltOfList : flatListFromLists | eltOfList <- list,
                                                               flatListFromLists <- allFlatLists lists]

getPossibleBoardsFrom :: Eq t => MultiBoard t -> [Board t]
getPossibleBoardsFrom mbrd = allFlatLists (map allFlatLists mbrd)
------------------------------------------------------

tightenBy :: Eq t => (MultiBoard t -> [BldgBlock [t]]) -> MultiBoard t -> MultiBoard t
tightenBy getBldgBlocks mbrd = combineBldgBlocks (map {-keepSifting-} siftOptions (getBldgBlocks mbrd))
                               where combineBldgBlocks = getBldgBlocks

tighten :: Eq t => MultiBoard t -> MultiBoard t
tighten mbrd = tightenBy subgridsOf (tightenBy columnsOf (tightenBy rowsOf mbrd))

keepTightening :: Eq t => MultiBoard t -> MultiBoard t
keepTightening = repeatUntilSame tighten

feasible :: Board Int -> Bool
feasible brd = all hasDistinctValues (rowsOf brd) &&
               all hasDistinctValues (columnsOf brd) &&
               all hasDistinctValues (subgridsOf brd)
               where hasDistinctValues [] = True
                     hasDistinctValues (int : ints) = int `notElem` ints && hasDistinctValues ints

solveSudoku :: Board Int -> [Board Int]
solveSudoku brd = filter feasible (getPossibleBoardsFrom (keepTightening {-tighten-} (createMxNoptions brd)))

-------------------------------------------------
-- Trying another way to solve in an attempt to make it faster

noRepeatedSinglesIn :: Eq t => BldgBlock [t] -> Bool
noRepeatedSinglesIn [] = True
noRepeatedSinglesIn (x:xs) = if length x == 1
                             then x `notElem` xs && noRepeatedSinglesIn xs
                             else noRepeatedSinglesIn xs

noImpossibleCellsIn :: Eq t => BldgBlock [t] -> Bool
noImpossibleCellsIn = not . any null

qualify :: Eq t => MultiBoard t -> Bool
qualify mbrd = let notHopeless x = noRepeatedSinglesIn x && noImpossibleCellsIn x
               in all notHopeless (rowsOf mbrd) && all notHopeless (columnsOf mbrd) && all notHopeless (subgridsOf mbrd)

hasOptions :: [[t]] -> Bool
hasOptions = any (\lst -> length lst > 1)

getPossibleBoardsForFirstNonSingle :: MultiBoard t -> [MultiBoard t]
getPossibleBoardsForFirstNonSingle lstlstlst = let (rowsWithAllSingles, firstRowWithOptions : remainingRows) = break hasOptions lstlstlst
                                               in let (singleCells, firstCellWithOptions : remainingCells) = break (\lst -> length lst == 1) firstRowWithOptions
                                                  in [rowsWithAllSingles ++ [singleCells ++ ([anOptionForTheCell] : remainingCells)] ++ remainingRows | anOptionForTheCell <- firstCellWithOptions]

anotherSolver :: Eq t => MultiBoard t -> [Board t]
anotherSolver mbrd | not (qualify mbrd) = []
                   | all (all (\lst -> length lst == 1)) mbrd = getPossibleBoardsFrom mbrd
                   | otherwise = concatMap anotherSolver (getPossibleBoardsForFirstNonSingle (keepTightening mbrd))
                    
solveSudoku2 :: Board Int -> [Board Int]
solveSudoku2 brd = anotherSolver (tighten (createMxNoptions brd))

--printSolutions
printBoards :: [String] -> IO ()
printBoards = mapM_ putStrLn

toBeSolved :: Board Int
toBeSolved = examples !! 2

main :: IO ()
main = do
    printBoards (map stringifyBoard [toBeSolved, head (solveSudoku toBeSolved)])