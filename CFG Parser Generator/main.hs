{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.List (find)
import Data.List.Split
import Data.Maybe (fromJust)

data CFG2 = CFG2 {
    start_ :: Char,
    rules_ :: Char -> [String],
    inv_rules_ :: String -> [Char]
}

-- Simple parenthesis-matching grammar
example1 :: CFG2
example1 = CFG2 {

    start_ = 'P',

    -- Non-CNF grammar for reference
    -- S -> (S) | SS | empty

    rules_ = \case 'P' -> ["S", ""]
                   'S' -> ["SS", "OB"]
                   'B' -> ["SC", ")"]
                   'O' -> ["("]
                   'C' -> [")"]
                   _ -> [],

    inv_rules_ = \case "" -> ['P']
                       "S" -> ['P']
                       "SS" -> ['S']
                       "OB" -> ['S']
                       "SC" -> ['B']
                       ")" -> ['B','C']
                       "(" -> ['O']
                       _ -> []
}

-- Even-length palindromes using the letters a,b,c
example2 :: CFG2
example2 = CFG2 {

    start_ = 'S',

    -- Non-CNF grammar for reference
    -- P -> aPa | bPb | cPc | QQ | empty
    -- Q -> aa | bb | cc

    rules_ = \case 'S' -> ["P", ""]
                   'P' -> ["AD","BE","CF"]
                   'D' -> ["PA","a"]
                   'E' -> ["PB","b"]
                   'F' -> ["PC","c"]
                   'A' -> ["a"]
                   'B' -> ["b"]
                   'C' -> ["c"],

    inv_rules_ = \case "" -> ['S']
                       "P" -> ['S']
                       "AD" -> ['P']
                       "BE" -> ['P']
                       "CF" -> ['P']
                       "PA" -> ['D']
                       "PB" -> ['E']
                       "PC" -> ['F']
                       "a" -> ['A','D']
                       "b" -> ['B','E']
                       "c" -> ['C','F']
                       _ -> []
}

-- Returns list of strings formed by the cartesian product of input pair of strings
cpStrings :: (String, String) -> [String]
cpStrings ("", _) = []
cpStrings (_, "") = []
cpStrings (s1,s2) = concatMap (\c1 -> map (\c2 -> [c1,c2]) s2) s1

-- CYK without backtracking
-- Just returns the variable (non-terminal) that the string can be parsed as.
-- This was written just to make sure that the core algo works well, before
-- moving on to the version with backtracking
runcyk :: CFG2 -> String -> Int -> Int -> String
runcyk cfg str beg end = if end == beg+1
                         then cfg.inv_rules_ [str !! beg]
                         else concatMap cfg.inv_rules_ (concatMap (cpStrings . (\mid -> (runcyk cfg str beg mid , runcyk cfg str mid end))) [beg+1 .. end-1])

-- Below is the actual CYK algorithm with memo table
-- I have maintained an immutable table, which essentially means after computing each sub-step, I get back a new table.

-- Just a helper to update a cell in a 2-D matrix
updateCellInTable :: [[[a]]] -> Int -> Int -> [a] -> [[[a]]]
updateCellInTable table i j lststr = let (beforei, nowi:afteri) = splitAt i table
                                         (beforej, _:afterj) = splitAt j nowi
                                     in beforei ++ ((beforej ++ (lststr:afterj)): afteri)

-- Type of the memo table
type CYKTable = [[[String]]]

-- Determines the sequence in which the cells in table need to be filled up so as
-- to ensure that sub-problems are already solved before we tackle the parent problem
getNextCellInTable :: Int -> Int -> Int -> [Int]
getNextCellInTable strlen i j
  | j==strlen-1 && i==0 = []
  | i==0 = [j+1, j+1]
  | otherwise = [i-1, j]

-- Function to generate all possible parse-pairs using solutions to sub-problems
-- and filter the valid ones to generate corresponding variables (non-terminals) from the grammar
joinSubParses :: CFG2 -> CYKTable -> Int -> Int -> Int -> String
joinSubParses cfg table beg end mid = let fh = filter (`notElem` "~") (concat ((table !! beg) !! (mid-1))) -- will be String
                                          sh = filter (`notElem` "~") (concat ((table !! mid) !! end))     -- will be String
                                          doubles = cpStrings (fh,sh)                      -- will be [String]
                                          nonterms = concatMap cfg.inv_rules_ doubles -- will be String
                                      in if null nonterms then "~" else nonterms

-- Driver function
-- Initializes emty table and calls cyk
parseWithBacktracking :: CFG2 -> String -> CYKTable
parseWithBacktracking cfg str = let n = length str
                                    table = replicate n (replicate n []) --replicate n (replicate n (replicate (n-1) "~"))
                                  in cyk cfg str 0 0 table

-- Core logic of CYK algorithm
cyk :: CFG2 -> String -> Int -> Int -> CYKTable -> CYKTable
cyk cfg str beg end table = let newTable = if beg == end
                                           then updateCellInTable table beg end [cfg.inv_rules_ [str !! beg]]
                                           else updateCellInTable table beg end (map (joinSubParses cfg table beg end) [beg+1 .. end])
                                nextCell = getNextCellInTable (length str) beg end
                            in if null nextCell then newTable else cyk cfg str (head nextCell) (nextCell !! 1) newTable

-- Data structure to build a parse tree
data Tree a = Leaf a a | Node a (Tree a) (Tree a)
type ParseTree = Tree Char

-- For console output
instance Show a => Show (Tree a) where
    show :: Show a => Tree a -> String
    show (Leaf var term) = "("++ show var ++ " (" ++ show term ++ "))"
    show (Node var t1 t2) = "("++ show var ++" "++show t1++" "++show t2++")"

-- Function to build a parse tree from the table populated by CYK
buildTree :: String -> CYKTable -> Int -> Int -> ParseTree
buildTree str table beg end = let entry = (table !! beg) !! end  -- will be [String]
                                  (chosenMid, midVars) = fromJust (find (\(idx,str) -> str /= "~") (zip [0 ..] entry)) -- will be (Int, String)
                                  midVar = head midVars -- will be Char
                              in if beg == end
                                 then Leaf midVar (str !! beg)
                                 else Node midVar (buildTree str table beg (beg + chosenMid)) (buildTree str table (beg + chosenMid + 1) end)

-- Function to process the parse tree bottom-up and generate XDOT syntax for visualization
generateXDOT :: ParseTree -> String -> Char -> [String]
generateXDOT (Leaf var term) parentName suffix = let selfName = parentName ++ [suffix]
                                                     termName = selfName ++ "0"
                                                     selfDecl = selfName ++ "[label=\"" ++ [var] ++ "\"]\n"
                                                     termDecl = termName ++ "[label=\"" ++ [term] ++ "\", fontcolor=\"red\", color=\"red\"]\n"
                                                     --selfEdge = parentName ++ " -> " ++ selfName
                                                     termEdge = selfName ++ " -> " ++ termName ++ "\n"
                                                 in [selfDecl, termDecl, termEdge]

generateXDOT (Node var t1 t2) parentName suffix = let selfName = parentName ++ [suffix]
                                                      selfDecl = selfName ++ "[label=\"" ++ [var] ++ "\"]\n"
                                                      selfEdge = parentName ++ " -> " ++ selfName
                                                      accFromLeft = generateXDOT t1 selfName '0'
                                                      leftName = head (splitOn "[" (head accFromLeft))
                                                      accFromRight = generateXDOT t2 selfName '1'
                                                      rightName = head (splitOn "[" (head accFromRight))
                                                      leftEdge = selfName ++ " -> " ++ leftName ++ "\n"
                                                      rightEdge = selfName ++ " -> " ++ rightName ++ "\n"
                                                  in --if parentName == ""
                                                     [selfDecl, leftEdge, rightEdge] ++ accFromLeft ++ accFromRight
                                                     --else [selfDecl, selfEdge, leftEdge, rightEdge] ++ accFromLeft ++ accFromRight

-- End-to-end function that takes CFG (in CNF) and a string, and outputs XDOT
getParseTreeInXDOT :: CFG2 -> String -> String
getParseTreeInXDOT cfg str = let cyktable = parseWithBacktracking cfg str
                                 ptree = buildTree str cyktable 0 (length str - 1)
                                 xdot = generateXDOT ptree "" 'n'
                             in "digraph G {\n" ++ concat xdot ++ "}"


-- Writes the generated XDOT to the "out.dot" file
main :: IO ()
main = do
    let xdotContents = getParseTreeInXDOT example3 "ccabacc"
    writeFile "./out.dot" xdotContents
    
