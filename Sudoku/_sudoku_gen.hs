import System.Random ( random, randomR )
import System.Random.Internal ( mkStdGen )
import System.Random.Shuffle ( shuffle' )
import Data.List (transpose, sortBy)
import System.Random.Stateful
import Control.Monad
import Data.Function

m :: Int
m = 3

n :: Int
n = 3

{-

See if you can combine the following with diagonal-first approach

Generate all possible combinations of [1..9]*9
For each combination, do mod 9 + 1 and check validity
if valid: delete random cells and return

-}

getOrderedListMxN :: Int -> Int -> [Int]
getOrderedListMxN m n = concat (replicate (m*n) [1..m*n])


--generateAllNums :: Int -> Int -> IO [Int]
--generateAllNums m n = replicateM (m*n) (randomRIO (1,m*n::Int))
rowsOf :: Board t -> [BldgBlock t]
rowsOf brd = brd

columnsOf :: Board t -> [BldgBlock t]
columnsOf = transpose

subgridsOf :: Board t -> [BldgBlock t]
subgridsOf [] = []
subgridsOf brd = getSubgridsInHorizontalDir (take n brd) ++ subgridsOf (drop n brd)
                 where getSubgridsInHorizontalDir brd@(x:_) =  if null x
                                                               then []
                                                               else concatMap (take m) brd : getSubgridsInHorizontalDir (map (drop m) brd)


type BldgBlock a = [a]
-- An BldgBlock can be a row, a column, or a sub-grid
type Board b = [BldgBlock b]

feasible :: Board Int -> Bool
feasible brd = all hasDistinctValues (rowsOf brd) &&
               all hasDistinctValues (columnsOf brd) &&
               all hasDistinctValues (subgridsOf brd)
               where hasDistinctValues [] = True
                     hasDistinctValues (int : ints) = int `notElem` ints && hasDistinctValues ints


--generate1toMxN :: Int -> Int -> [Int]
--generate1toMxN m n = {-take (m*n)-} (foldl (\acc num -> let rdm = randomR (1, m*n) (mkStdGen (m*n*m*n))
--                                                    in if fst rdm `notElem` acc then fst rdm:acc else acc) [] (replicate 600 1))

--generate1toMxN :: Int -> Int -> [Int]
--generate1toMxN m n = shuffle' [1..m*n] (m*n) (mkStdGen (fst $ random (mkStdGen 1024)))


-- THIS WORKS. DO SOMETHING WITH THIS / LIKE THIS.
randomize :: [a] -> IO [a]
randomize lst = do
                rands <- replicateM (length lst) $ randomRIO (1 :: Int, 100000)
                pure $ map fst $ sortBy (compare `on` snd) (zip lst rands)

--boardify :: IO [a] -> Int -> Int -> IO [[a]]

boardify :: IO [a] -> Int -> Int -> IO [[a]]
boardify ionums m n = do
      nums <- ionums
      return (take (m*n) nums : boardify (drop (m*n) nums) m n)

main :: IO ()
main = do
  putStrLn "not randomized"
  let nums = [1..10]
  print nums
  putStrLn "randomized"
  print =<< randomize nums