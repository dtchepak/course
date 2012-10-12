{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module TicTacToe.Game where
{-
* `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is in-play. Calling move on a game board that is finished is a *compile-time type error*.
* `whoWon`: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither). This function can only be called on a board that is finished. Calling move on a game board that is in-play is a *compile-time type error*.
* `takeBack`: takes either a finished board or a board in-play that has had at least one move and returns a board in-play. It is a compile-time type error to call this function on an empty board.
* `playerAt`: takes a tic-tac-toe board and position and returns the (possible) player at a given position. This function works on any type of board.
 -}
import Data.List

data InPlayBoard = InPlayBoard Moves  -- or InPlayBoard Position Player Moves, so no empty list case
    deriving (Eq, Show)
data NewBoard = NewBoard deriving (Eq, Show)
data FinishedBoard = FinishedBoard Moves
    deriving (Eq, Show)

data Position = 
    NW |  N  | NE |
    W  |  C  |  E |
    SW |  S  | SE
    deriving (Eq, Show, Enum, Ord, Bounded)

data Move = Move { pos :: Position, player :: Player } deriving (Show, Eq)
type Moves = [Move]
data Player = Naught | Cross
    deriving (Show, Eq)

data MoveResult = PositionOccupied | Next InPlayBoard | GameOver FinishedBoard
    deriving (Show, Eq)

class Movable from to | from -> to where
    move :: Position -> from -> to

instance Movable NewBoard InPlayBoard where
    move p b = InPlayBoard [Move p whoGoesFirst]

instance Movable InPlayBoard MoveResult where
    move p b@(InPlayBoard moves) = 
        maybe (boardFrom (makeMove p moves)) (const PositionOccupied) (playerAt p b)
        where
            boardFrom :: Moves -> MoveResult
            boardFrom m = if isFinished m 
                              then GameOver $ FinishedBoard m
                              else Next $ InPlayBoard m

data TakenBack = Restart NewBoard | BackTo InPlayBoard
    deriving (Eq, Show)

class TakeBack from to | from -> to where
    takeBack :: from -> to

instance TakeBack FinishedBoard InPlayBoard where
    takeBack (FinishedBoard (m:ms)) = InPlayBoard ms

instance TakeBack InPlayBoard TakenBack where
    takeBack (InPlayBoard [m]) = Restart NewBoard
    takeBack (InPlayBoard (m:ms)) = BackTo $ InPlayBoard ms

class Board b where
    playerAt :: Position -> b -> Maybe Player
    moves :: b -> Moves
instance Board InPlayBoard where
    playerAt p (InPlayBoard moves) = fmap player . findMove p $ moves
    moves (InPlayBoard m) = m
instance Board NewBoard where
    playerAt _ _ = Nothing
    moves _ = []
instance Board FinishedBoard where
    playerAt p (FinishedBoard moves) = fmap player . findMove p $ moves
    moves (FinishedBoard m) = m

whoWon :: FinishedBoard -> Maybe Player
whoWon (FinishedBoard moves)
    | hasWin $ positions Cross = Just Cross
    | hasWin $ positions Naught = Just Naught
    | otherwise = Nothing
    where positions p = map pos . filter ((p==) . player) $ moves

findMove :: Position -> Moves -> Maybe Move
findMove p  = find ((p==) . pos) 

whoGoesFirst :: Player
whoGoesFirst = Cross

whoIsNext :: Moves -> Player
whoIsNext [] = whoGoesFirst
whoIsNext (x:_) = if player x == Cross then Naught else Cross

makeMove :: Position -> Moves -> Moves
makeMove pos moves = let pl = whoIsNext moves in (Move pos pl):moves

isFinished :: Moves -> Bool
isFinished ms = case whoWon (FinishedBoard ms) of
                    Just _ -> True
                    Nothing -> length ms == length [NW .. SE]

hasWin :: [Position] -> Bool
hasWin p = any (all (`elem` p)) wins

wins :: [[Position]]
wins = [ 
      [NW, N, NE],  --horizontal wins
      [W , C, E ],
      [SW, S, SE],
      [NW, W, SW],  --vertical wins
      [N,  C, S ],
      [NE, E, SE],
      [NW, C, SE],  --diagonal wins
      [SW, C, NE] ] 

foldMoveResult ::
    a --position occupied
    -> (InPlayBoard -> a) --continuing game
    -> (FinishedBoard -> a) -- finished game
    -> MoveResult
    -> a
foldMoveResult occ _ _ PositionOccupied = occ
foldMoveResult _ next _ (Next b) = next b
foldMoveResult _ _ finished (GameOver b) = finished b

keepPlayingOr :: 
    MoveResult             -- current position
    -> (InPlayBoard -> a)  -- action to take if can keep playing
    -> a                   -- value to return if cannot keep playing
    -> a
keepPlayingOr cur action def = foldMoveResult def action (const def) cur

foldTakenBack :: 
    (NewBoard -> a) -- restart
    -> (InPlayBoard -> a) -- back to
    -> TakenBack
    -> a
foldTakenBack new _ (Restart b) = new b
foldTakenBack _ backTo (BackTo b) = backTo b

