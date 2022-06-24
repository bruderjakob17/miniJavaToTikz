import System.Environment

import Data.List
import Data.Char
import Data.Function
import qualified Data.Maybe as Maybe

trim = dropWhileEnd isSpace . dropWhile isSpace

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

replaceAll :: String -> String -> String -> String
replaceAll t r [] = []
replaceAll t r s = if t `isPrefixOf` s then r ++ replaceAll t r (drop (length t) s) else head s : replaceAll t r (tail s)

data Statement =
    EmptyStatement
    | Assign VName Expr
    | Read VName
    | Write Expr
    | If Condition Statement
    | IfElse Condition Statement Statement
    | While Condition Statement
    | Seq Statement Statement
    deriving (Show)
{-data Expr =
    N Number
    | V VName
    | AExp1 Unop Expr
    | AExp2 Binop Expr Expr
    deriving (Show)-}
type Expr = String
{-data Condition =
    CTrue
    | CFalse
    | Comparison Comp Condition Condition
    | BExp1 Bunop Condition
    | BExp2 Bbinop Condition Condition
    deriving (Show)-}
type Condition = String
{-data Comp =
    Eq
    | Uneq
    | Leq
    | Le
    | Geq
    | Ge
    deriving (Show)
data Unop = Unminus
    deriving (Show)
data Binop = Sub | Add | Mul | Div | Mod
    deriving (Show)
data Bunop = Not
    deriving (Show)
data Bbinop = And | Or
    deriving (Show)-}

type VName = String
-- type Number = Integer

-- estimates the height needed in a tikzpicture for a given statement
-- (estimated in approx. number of single instructions / nodes)
height :: Statement -> Int
height EmptyStatement = 0
height (Assign _ _) = 1
height (Read _) = 1
height (Write _) = 1
height (Seq c1 c2) = height c1 + height c2
height (If b c) = 2 + height c
height (IfElse b c1 c2) = 2 + max (height c1) (height c2)
height (While b c) = 3 + height c

-- estimates the width needed in a tikzpicture for a given statement
width :: Statement -> Int
width EmptyStatement = 0
width (Assign x v) = length x + length " = " + length v + length ";"
width (Read x) = length x + length " = read();"
width (Write x) = length "write();" + length x
width (Seq c1 c2) = max (width c1) (width c2)
width (If b c) = width (IfElse b c EmptyStatement)
width (IfElse b c1 c2) = max (width c1) 30 + max (width c2) 30 + length b
width (While b c) = length b + width c + 30

-- Contains necessary context information needed to convert a statement into a tikzpicture.
-- A conversion state ((i, s), l) means:
-- i is the lowest free index that can be used for node identifiers, e.g. if the conversion state is ((3, "node1"), []), "node3" can be used.
-- s is the parent node from which the flow is coming. In the example, this would be "node1".
-- l is a list accumulating all node/edge definitions so far.
type ConversionState = ((Integer, String), [String])

toSequence :: [Statement] -> Statement
toSequence [] = EmptyStatement
toSequence [c] = c
toSequence (c:cs) = Seq c (toSequence cs)

-- ((i, s), l) as parameter: i: free index, s: parent node name, l: accumulator (reversed order)
convert :: ConversionState -> Statement -> ConversionState
convert ((i, s), l) EmptyStatement = ((i, s), l)
convert ((i, s), l) (Assign x v) =
  let node_name = "node" ++ show i
   in ( (i + 1, node_name),
        ("\\path[->] (" ++ s ++ ") edge (" ++ node_name ++ ");") :
        ("\\node[below=of "
          ++ s
          ++ ", rectangle, draw]("
          ++ node_name
          ++ ") {\\texttt{"
          ++ x
          ++ " = "
          ++ v
          ++ ";}};") :
        l
      )
convert ((i, s), l) (Read x) =
  let node_name = "node" ++ show i
   in ( (i + 1, node_name),
        ("\\path[->] (" ++ s ++ ") edge (" ++ node_name ++ ");") :
        ("\\node[below=of "
          ++ s
          ++ ", rectangle, draw]("
          ++ node_name
          ++ ") {\\texttt{"
          ++ x
          ++ " = read();}};") :
        l
      )
convert ((i, s), l) (Write x) =
  let node_name = "node" ++ show i
   in ( (i + 1, node_name),
        ("\\path[->] (" ++ s ++ ") edge (" ++ node_name ++ ");") :
        ("\\node[below=of "
          ++ s
          ++ ", rectangle, draw]("
          ++ node_name
          ++ ") {\\texttt{write("
          ++ x
          ++ ");}};") :
        l
      )
convert ((i, s), l) (If b c) = convert ((i, s), l) (IfElse b c EmptyStatement)
convert ((i, s), l) (IfElse b c1 c2) =
  let branch_name = "node" ++ show i
   in let left_name = "node" ++ show (i + 1)
       in let ((i_left, s_left), l_left) =
                convert
                  ( (i + 2, left_name),
                    ( "\\path[-] (" ++ branch_name ++ ") edge node [above] {yes} (" ++ left_name ++ ");" ) :
                    ( "\\node[left=of "
                        ++ branch_name
                        ++ ", coordinate]("
                        ++ left_name
                        ++ ") {};"
                    ) :
                    ( "\\path[->] (" ++ s ++ ") edge (" ++ branch_name ++ ");" ) :
                    ( "\\node[below=of "
                        ++ s
                        ++ ", diamond, draw]("
                        ++ branch_name
                        ++ ") {\\texttt{"
                        ++ b
                        ++ "}};"
                    ) :
                    l
                  )
                  c1
           in let right_name = "node" ++ show i_left
            in let ((i_right, s_right), l_right) = convert ((i_left+1, right_name), ( "\\path[-] (" ++ branch_name ++ ") edge node [above] {no} (" ++ right_name ++ ");" ) :
                    ( "\\node[right=of "
                        ++ branch_name
                        ++ ", coordinate]("
                        ++ right_name
                        ++ ") {};"
                    ) :
                    l_left) c2
              in let join_name = "node" ++ show i_right in let right_bot_name = "node" ++ show (i_right + 1) in let left_bot_name = "node" ++ show (i_right + 2)
               in ( (i_left + 3, join_name),
                    ( "\\path[->] (" ++ right_bot_name ++ ") edge (" ++ join_name ++ ");" ) :
                    ( "\\path[->] (" ++ left_bot_name ++ ") edge (" ++ join_name ++ ");" ) :
                    ( "\\path[-] (" ++ s_right ++ ") edge (" ++ right_bot_name ++ ");" ) :
                    ( "\\path[-] (" ++ s_left ++ ") edge (" ++ left_bot_name ++ ");" ) :
                    ( "\\draw let \\p1=(" ++ s_left ++ ") in let \\p2=(" ++ join_name ++ ") in node[coordinate](" ++ left_bot_name ++ ") at (\\x1, \\y2) {};" ) :
                    ( "\\draw let \\p1=(" ++ s_right ++ ") in let \\p2=(" ++ join_name ++ ") in node[coordinate](" ++ right_bot_name ++ ") at (\\x1, \\y2) {};" ) :
                    ( "\\draw let \\p1=(" ++ s_left ++ ") in let \\p2=(" ++ s_right ++ ") in node[draw, circle](" ++ join_name ++ ") at (.5*\\x1 + .5*\\x2, \\y" ++ (if height c1 > height c2 then "1" else "2") ++ "-10mm) {};" ) :
                    l_right
                  )
convert ((i, s), l) (While b c) =
  let join_name = "node" ++ show i in
    let branch_name = "node" ++ show (i+1) in
      let right_name = "node" ++ show (i+2) in
        let left_name = "node" ++ show (i+3) in
          let ((i_loop, s_loop), l_loop) =
                convert ((i+4, right_name),
                  ( "\\path[-] (" ++ branch_name ++ ") edge node [above] {yes} (" ++ right_name ++ ");" ) :
                  ( "\\path[-] (" ++ branch_name ++ ") edge node [above] {no} (" ++ left_name ++ ");" ) :
                  ( "\\node[left=of " ++ branch_name ++ ", coordinate](" ++ left_name ++ ") {};" ) :
                  ( "\\node[right=of " ++ branch_name ++ ", coordinate](" ++ right_name ++ ") {};" ) :
                  ( "\\path[->] (" ++ join_name ++ ") edge (" ++ branch_name ++ ");" ) :
                  ( "\\node[below=of " ++ join_name ++ ", draw, diamond](" ++ branch_name ++ ") {\\texttt{" ++ b ++ "}};" ) :
                  ( "\\path[->] (" ++ s ++ ") edge (" ++ join_name ++ ");" ) :
                  ( "\\node[below=of " ++ s ++ ", draw, circle](" ++ join_name ++ ") {};" ) :
                  l
                ) c
                in let corner0 = "node" ++ show i_loop in let corner1 = "node" ++ show (i_loop + 1) in let corner2 = "node" ++ show (i_loop + 2) in
                  ((i_loop + 3, left_name),
                  ( "\\path[->] (" ++ corner2 ++ ") edge (" ++ join_name ++ ");" ) :
                  ( "\\path[-] (" ++ corner1 ++ ") edge (" ++ corner2 ++ ");" ) :
                  ( "\\path[-] (" ++ corner0 ++ ") edge (" ++ corner1 ++ ");" ) :
                  ( "\\path[-] (" ++ s_loop ++ ") edge (" ++ corner0 ++ ");" ) :
                  ("\\draw let \\p1 = (" ++ corner1 ++ ") in let \\p2 = (" ++ join_name ++ ") in node[coordinate](" ++ corner2 ++ ") at (\\x1, \\y2) {};") :
                  ("\\node[right=" ++ show (max ((width c & fromIntegral) / 10.0) 2.0) ++ "cm of " ++ corner0 ++ ", coordinate](" ++ corner1 ++ ") {};") :
                  ("\\node[below=of " ++ s_loop ++ ", coordinate](" ++ corner0 ++ ") {};") :
                  l_loop)
convert ((i, s), l) (Seq c1 c2) = let ((i2, s2), l2) = convert ((i, s), l) c1 in convert ((i2, s2), l2) c2

convertComplete :: Statement -> [String]
convertComplete c =
  let ((i, s), l) = convert ((0, "start"), ["\\node[circle, draw](start){Start};"]) c in reverse (("\\path[->] (" ++ s ++ ") edge (end);") : ("\\node[below=of " ++ s ++ ", circle, draw](end){End};") : l)

outputTex :: String -> Statement -> IO ()
outputTex filename statement = do
  writeFile (filename ++ ".tex") (unlines (["\\documentclass{article}","\\usepackage{tikz}","\\usetikzlibrary{calc, decorations.pathmorphing, shapes.geometric, arrows, positioning}","\\begin{document}","\\begin{center}"] ++ (["\\resizebox{!}{\\textheight}{" | height statement >= 12]) ++ ["\\begin{tikzpicture}"] ++ convertComplete statement ++ ["\\end{tikzpicture}"] ++ (["}" | height statement >= 12]) ++ ["\\end{center}","\\end{document}"]))

splitAtTopLevelAux :: Char -> (String, String) -> Integer -> Maybe (String, String)
splitAtTopLevelAux c (l, r) i = case r of
  [] ->  Nothing
  x : xs -> if x == c && i == 0 then Just (reverse l, xs) else splitAtTopLevelAux c (x : l, xs) (if x `elem` "({[" then i + 1 else if x `elem` ")}]" then i - 1 else i)

splitAtTopLevelAux2 :: String -> (String, String) -> Integer -> Maybe (String, String)
splitAtTopLevelAux2 s (l, r) i = if i == 0 && s `isPrefixOf` r then Just (reverse l, stripPrefix s r & Maybe.fromMaybe r) else case r of
  [] ->  Nothing
  x : xs -> splitAtTopLevelAux2 s (x : l, xs) (if x `elem` "({[" then i + 1 else if x `elem` ")}]" then i - 1 else i)

splitTopLevel :: Char -> String -> [String]
splitTopLevel c s = case splitAtTopLevelAux c ("", s) 0 of
  Nothing -> [s]
  Just (l, r) -> trim l : splitTopLevel c r

parseAsRead :: String -> Statement
parseAsRead s = Read (wordsWhen (=='=') s & head & trim)

parseAsWrite :: String -> Statement
parseAsWrite s = stripPrefix "write(" s & Maybe.fromMaybe s & init & Write

parseAsControlFlow :: String -> Statement
parseAsControlFlow s = -- after the while/if keyword, a condition is expected in parentheses
  let (l1,r1) = splitAtTopLevelAux '(' ("", s) 0 & Maybe.fromMaybe ("", s) in
    let (l2,r2) = splitAtTopLevelAux ')' ("", r1) 0 & Maybe.fromMaybe ("", s) in
      if "while" `isPrefixOf` trim s
      then
      -- l2 is the condition, and r2 contains the body.
      While (trim l2) (parseStatement (trim r2))
      else if "if" `isPrefixOf` trim s
        then case splitAtTopLevelAux2 "else" ("", r2) 0 of
          Nothing -> If (trim l2) (parseStatement (trim r2))
          Just (c1, c2) -> IfElse (trim l2) (parseStatement (trim c1)) (parseStatement (trim c2))
        else EmptyStatement

parseStatement :: String -> Statement
parseStatement s = let l = splitTopLevel ';' s in
  case length l of
    0 -> EmptyStatement
    1 ->
      -- try to remove brackets
      if "{" `isPrefixOf` s then drop 1 (take (length s - 1) s) & parseStatement
      -- try to parse read
      else if "read()" `isInfixOf` s then parseAsRead s
      -- try to parse write
      else if "write(" `isPrefixOf` s then parseAsWrite s
      -- try to parse assign
      else case splitAtTopLevelAux '=' ("", s) 0 of
        Just (l, r) -> Assign (trim l) (trim r)
        Nothing ->
          -- try to parse if, ifelse, while TODO (use splitAtTopLevelAux2)
            parseAsControlFlow s
    _ -> toSequence $ map parseStatement l

createTikz :: String -> IO ()
createTikz s = do
  contents <- readFile s
  print $ parseStatement contents
  outputTex s (parseStatement contents)

main = do
    args <- getArgs
    case args of
      [] -> putStr "Syntax: miniJavaToTikz <filename>"
      s : _ -> createTikz s