import Data.List

data Tile = X | O | Empty deriving (Eq, Ord)

instance Show Tile where
  show X = "X"
  show O = "O"
  show Empty = " "

data Player = PlayerX | PlayerO deriving (Eq, Show)

playerToSquare :: Player -> Tile
playerToSquare PlayerX = X
playerToSquare PlayerO = O

nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO =  PlayerX

type Board = [Char]

showBoard :: Board -> String
showBoard str =
  "   |   |   \n" ++
  " " ++ str !! 0 : [] ++ " | " ++ str !! 1 : [] ++ " | " ++ str !! 2 : []  ++ " \n" ++
  "   |   |  \n" ++
  "---+---+---\n" ++
  "   |   |   \n" ++
  " " ++ str !! 3 : [] ++ " | " ++ str !! 4 : [] ++ " | " ++ str !! 5 : []  ++ " \n" ++
  "   |   |   \n" ++
  "---+---+---\n" ++
  "   |   |   \n" ++
  " " ++ str !! 6 : [] ++ " | " ++ str !! 7 : [] ++ " | " ++ str !! 8 : []  ++ " \n" ++
  "   |   |   \n"

-- move
-- Given a board, a player and a move position, returns a new board with the
-- new move applied
move :: Board -> Char -> Int -> Board
move (p:board) ch pos
  | pos > 0 = p:[] ++ (move board ch (pos - 1))
  | otherwise = ch:[] ++ board

playerMoveX :: Board -> Int -> (Bool, Board)
playerMoveX board pos = (True, (move board 'X' pos))

playerMoveO :: Board -> Int -> (Bool, Board)
playerMoveO board pos = (True, (move board 'O' pos))

winner :: Board -> Char
winner b
  -- horizontal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 1) && (b !! 0) == (b !! 2)) = b !! 0
  | (b !! 3) /= ' ' && ((b !! 3) == (b !! 4) && (b !! 3) == (b !! 5)) = b !! 3
  | (b !! 6) /= ' ' && ((b !! 6) == (b !! 7) && (b !! 6) == (b !! 8)) = b !! 6
  -- vertical lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 3) && (b !! 0) == (b !! 6)) = b !! 0
  | (b !! 1) /= ' ' && ((b !! 1) == (b !! 4) && (b !! 1) == (b !! 7)) = b !! 1
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 5) && (b !! 2) == (b !! 8)) = b !! 2
  -- diagonal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 4) && (b !! 0) == (b !! 8)) = b !! 0
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 4) && (b !! 2) == (b !! 6)) = b !! 2
  -- no winner
  | otherwise = ' '

--instance Show Board where
  --show (Board x) = '\n' : (intercalate "\n-----\n" [intercalate "|" (map show row) | row <- x] ++ "\n")

  {-newBoard :: Board
--newBoard = Board (replicate 3 (replicate 3 Empty))
newBoard = Board [["8",Empty, Empty],
                               [Empty,Empty,Empty],
                               [Empty,Empty,Empty]]-}

{-spans :: Board -> [[Tile]]
spans (Board b) = b ++ transpose b ++ diags
                   where diags = [[b!!i!!i | i <- [0..(length b) - 1]],[b!!i!!((length b)-i-1) | i <- [0..(length b) - 1]]]-}

{-checkWinner :: Board -> Bool
checkWinner board
  | board == Board [[Empty,Empty,Empty], [Empty,Empty,Empty], [Empty,Empty,Empty]] = True
  | otherwise = False-}


play :: Board -> Player -> IO ()
play board player = do
  putStrLn ( showBoard board )
  if ((winner board) /= ' ')
    then do
      putStrLn("Winner " ++ (show (winner board) ));
    else do
      putStrLn "Play? "
      pos <- getLine
      let (valid, b) = (playerMoveX board (read pos) )
      then do putStrLn("\nOk\n");
                      if (' ' /= (winner b))
                        then do
                          putStrLn("Winner " ++ (show (winner b) ));
                          putStrLn ( showBoard b )
                        else do
                          -- putStrLn( show (scoreMoves b) )
                          -- putStrLn( show (bestMove b) )
                          play (move b 'O' (bestMove b))
                  
        

main = do
  putStrLn "Tic Tac Toe!\n"
  play "012345678" PlayerX