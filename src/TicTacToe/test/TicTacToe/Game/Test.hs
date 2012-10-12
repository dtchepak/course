module TicTacToe.Game.Test where

import Test.QuickCheck
import TicTacToe.Game

instance Arbitrary Position where
    arbitrary = elements [NW .. SE]

instance Arbitrary InPlayBoard where
    arbitrary = do
        firstPos <- arbitrary
        positions <- arbitrary
        let initialMove = move firstPos NewBoard
        return $ foldr (\p current -> keepPlayingOr (move p current) id current) initialMove positions
prop_oneMoveThenTakeBackShouldReturnToNewGame p =
    (takeBack . move p) NewBoard == Restart NewBoard

prop_moveThenTakeBackShouldRestorePreviousGame :: 
    InPlayBoard 
    -> Position 
    -> Bool
prop_moveThenTakeBackShouldRestorePreviousGame board p =
    let checkTakenBackResult moved = foldTakenBack (const False) (==board) (takeBack moved)
    in foldMoveResult True checkTakenBackResult ((board ==) . takeBack) (move p board)

