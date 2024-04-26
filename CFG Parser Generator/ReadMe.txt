This project is a CFG parser generator.
The input CFG has to be in Chomsky Normal Form (because I am using the CYK algorithm).

Prerequisites:

1. Haskell's 'split' library
   can be installed using "cabal install --lib split"

2. GraphViz (DOT) tool for visualization of parse tree

How to run:

1. Input

   Unfortunately, the CFG has to be hardcoded in the Haskell source (main.hs).
   Based on how difficult Haskell made input processing for the Economancy game,
      I did not include user input CFG in the scope for this project.
   The main.hs file has two example CFGs already converted in the CNF style.
      example1 : Parenthesis matching
      example2 : Even-length palindromes using letters a,b,c
   Any desired CFG (has to be in CNF) can be plugged in as another example using the same template.
   I have given at the end of this file a CFG for odd-length palindromes using the letters a,b,c.

2. Run

   Replace the CFG and the string to be parsed as arguments to the call to 'getParseTreeInXDOT' in the 'main' function.
   Compile the file using "ghc main.hs"
   An executable file will be generated. Run the executable.

2. Output

   If the input string parses successfully to any non-terminal in the grammar, the out.dot file will be generated.
   Otherwise, the run will error out.
   To see the parse tree, run "dot -Tsvg .\out.dot > out.svg"
   This will generate the out.svg image which shows the parse tree.
   (I did not have to perform this step on my machine because VSCode has a plugin that
     automatically renders graphs from .dot files.)
   NOTE:
   Black nodes in the tree represent non-terminals in the grammar and
   red leaves of the tree represent the actual symbols in the string being parsed.
   

Example CFG:

-- This can be copy-pasted in the source
-- Odd-length palindromes using the letters a,b,c
example3 :: CFG2
example3 = CFG2 {

    start_ = 'S',

    -- Non-CNF grammar for reference
    -- P -> aPa | bPb | cPc | Q
    -- Q -> a | b | c

    rules_ = \case 'S' -> ["P"]
                   'P' -> ["DA","EB","FC","a","b","c"]
                   'D' -> ["AP"]
                   'E' -> ["BP"]
                   'F' -> ["CP"]
                   'A' -> ["a"]
                   'B' -> ["b"]
                   'C' -> ["c"],

    inv_rules_ = \case "P" -> ['S']
                       "AP" -> ['D']
                       "BP" -> ['E']
                       "CP" -> ['F']
                       "DA" -> ['P']
                       "EB" -> ['P']
                       "FC" -> ['P']
                       "a" -> ['A','P']
                       "b" -> ['B','P']
                       "c" -> ['C','P']
                       _ -> []
}
