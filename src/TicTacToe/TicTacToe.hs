{-
* `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is in-play. Calling move on a game board that is finished is a *compile-time type error*.

* `whoWon`: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither). This function can only be called on a board that is finished. Calling move on a game board that is in-play is a *compile-time type error*.

* `takeBack`: takes either a finished board or a board in-play that has had at least one move and returns a board in-play. It is a compile-time type error to call this function on an empty board.

* `playerAt`: takes a tic-tac-toe board and position and returns the (possible) player at a given position. This function works on any type of board.

* Other API functions that you may see fit. These can be determined by also writing an interactive console application that uses the API -- other useful functions are likely to arise.

You should write automated tests for your API. For example, the following universally quantified property holds true:

`forall Board b. forall Position p. such that (not (positionIsOccupied
p b)). takeBack(move(p, b)) == b`
 -}
import Data.List

data Player = Naught | Cross deriving (Eq)
data NewBoard = NewBoard
data InPlayBoard = InPlayBoard Moves
data FinishedBoard = FinishedBoard Moves
data Position = TopLeft | TopMiddle | TopRight | MiddleLeft | Middle | MiddleRight | BottomLeft | BottomMiddle | BottomRight deriving (Show,Eq,Enum,Read)
type Moves = [(Position,Player)]

class Board b where 
    moves :: b -> Moves
instance Board NewBoard where
    moves = const []
instance Board InPlayBoard where
    moves (InPlayBoard b) = b
instance Board FinishedBoard where
    moves (FinishedBoard b) = b
instance Show NewBoard where
    show b = showMoves []
instance Show InPlayBoard where
    show (InPlayBoard b) = showMoves b
instance Show FinishedBoard where
    show (FinishedBoard b) = showMoves b

instance Show Player where
    show Naught = "O"
    show Cross  = "X"

move :: Either NewBoard InPlayBoard -> Position -> Either FinishedBoard InPlayBoard
move (Left b) p = Right (InPlayBoard [(p, Naught)])
move (Right b) p
    | isFinished moves' = Left (FinishedBoard moves')
    | otherwise         = Right (InPlayBoard moves')
    where 
        currentMoves = moves b
        moves' = addMove currentMoves p
        isFinished m = False

whoWon :: FinishedBoard -> Player
whoWon = error "todo"

takeBack :: Either FinishedBoard InPlayBoard -> Either NewBoard InPlayBoard
takeBack = error "todo"

playerAt :: Board b => b -> Position -> Maybe Player
playerAt b = (flip lookup) (moves b)

--- GAME LOOP

main :: IO()
main = do
    play (Left NewBoard)

play :: Either NewBoard InPlayBoard -> IO()
play b = do
    pos <- fmap read getLine
    let nextBoard = move b pos
    putStrLn $ showBoard nextBoard
    nextTurn nextBoard

nextTurn :: Either FinishedBoard InPlayBoard -> IO()
nextTurn (Left x) = putStrLn "FINISHED"
nextTurn (Right x) = play (Right x)

---- HELPERS
whoseTurn :: Moves -> Player
whoseTurn [] = Cross
whoseTurn ((_,lastPlayer):_)
    | lastPlayer == Cross = Naught
    | otherwise           = Cross

isOccupied :: Moves -> Position -> Bool
isOccupied moves p = any (\(pos,_) -> pos == p) moves

addMove :: Moves -> Position -> Moves
addMove moves pos
    | isOccupied moves pos = moves
    | otherwise            = (pos, player):moves
    where player = whoseTurn moves

showMoves :: Moves -> String
showMoves m = (unlines . map show . splitAtN 3) positions
    where positions = map (playerAt (InPlayBoard m)) $ enumFromTo TopLeft BottomRight

showBoard :: (Board a, Board b, Show a, Show b) => Either a b -> String
showBoard (Left x) = show x
showBoard (Right x) = show x


splitAtN :: Int -> [a] -> [[a]]
splitAtN _ [] = []
splitAtN n list = first : splitAtN n rest
    where (first,rest) = splitAt n list
