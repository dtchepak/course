module Network.Server.TicTacToe.Game where

import Data.TicTacToe
import Network.Server.Common.Env
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Ref
import Network.Server.TicTacToe.Loop
import Data.Char(isSpace, toLower, toUpper)
import Data.List(intercalate)
import Data.Function(on)
import Data.IORef(readIORef, atomicModifyIORef)
import Data.Maybe(fromMaybe)
import Data.Foldable(msum, find)
import Data.Set(Set)
import Control.Applicative((<$), (<$>))
import System.IO(hGetLine, hPutStrLn)

data Unfinished =
    ZeroMoves
    | OneOrMoreMoves Board
    deriving (Eq, Show)

foldUnfinished :: a -> (Board -> a) -> Unfinished -> a
foldUnfinished a _ ZeroMoves = a
foldUnfinished _ f (OneOrMoreMoves b) = f b

type FinishedGames =
  [FinishedBoard]

type Game a =
  IORefLoop Unfinished (Unfinished, FinishedGames) a

data Command =
  Move Position
  | Current
  | Finished
  | Chat String
  | Turn
  | At Position
  | Unknown String
  deriving (Eq, Show)

-- |
--
-- >>> command "MOVE ne"
-- Move NE
--
-- >>> command "MOVE 2"
-- Move N
--
-- >>> command "GAME"
-- Current
--
-- >>> command "FiniSHED"
-- Finished
--
-- >>> command "CHAT hi"
-- Chat "hi"
--
-- >>> command "Turn"
-- Turn
--
-- >>> command "At 4"
-- At W
--
-- >>> command "At C"
-- At C
--
-- >>> command "At X"
-- Unknown "At X"
--
-- >>> command "Move i"
-- Unknown "Move i"

-- |
--
-- >>> command "MOVE ne"
-- Move NE
--
-- >>> command "MOVE 2"
-- Move N
--
-- >>> command "GAME"
-- Current
--
-- >>> command "FiniSHED"
-- Finished
--
-- >>> command "CHAT hi"
-- Chat "hi"
--
-- >>> command "Turn"
-- Turn
--
-- >>> command "At 4"
-- At W
--
-- >>> command "At C"
-- At C
--
-- >>> command "At X"
-- Unknown "At X"
--
-- >>> command "Move i"
-- Unknown "Move i"
command ::
  String
  -> Command
command z =
  let p l = reverse . dropWhile isSpace . reverse . dropWhile isSpace <$> prefixThen ((==) `on` toLower) l z
  in Unknown z `fromMaybe` msum [
                                  do m <- p "MOVE "
                                     q <- sPosition m
                                     return (Move q)
                                , Current <$ p "GAME"
                                , Finished <$ p "FINISHED"
                                , Chat <$> p "CHAT"
                                , Turn <$ p "TURN"
                                , do a <- p "AT"
                                     q <- sPosition a
                                     return (At q)
                                ]

-- |
--
-- >>> sPosition "1"
-- Just NW
--
-- > sPosition "E"
-- Just E
--
-- > sPosition "sw"
-- Just SW
--
-- > sPosition "x"
-- Nothing
sPosition ::
  String
  -> Maybe Position
sPosition s =
  let table = [
                (
                  ["1", "NW"]
                , NW
                )
              , (
                  ["2", "N"]
                , N
                )
              , (
                  ["3", "NE"]
                , NE
                )
              , (
                  ["4", "W"]
                , W
                )
              , (
                  ["5", "C"]
                , C
                )
              , (
                  ["6", "E"]
                , E
                )
              , (
                  ["7", "SW"]
                , SW
                )
              , (
                  ["8", "S"]
                , S
                )
              , (
                  ["9", "SE"]
                , SE
                )
              ]
      toUppers = map toUpper
  in fmap snd . find (\(t, _) -> elem (toUppers s) (toUppers <$> t)) $ table

currentUnfinished ::
  Game Unfinished
currentUnfinished =
  initLoop $ \env ->
    readIORef (envvalL `getL` env)

withCurrentUnfinished ::
  (Unfinished -> (Unfinished, a))
  -> Game a
withCurrentUnfinished f =
  initLoop $ \env ->
    atomicModifyIORef (envvalL `getL` env) f

withCurrentBoard :: a -> (Board -> a) -> Game a
withCurrentBoard a f =
    foldUnfinished a f <$> currentUnfinished

lastUnfinished ::
  Game Unfinished
lastUnfinished =
  Loop $ \_ (s, t) ->
    return (s, (s, t))

putUnfinished ::
  Unfinished
  -> Game ()
putUnfinished s =
  Loop $ \_ (_, t) ->
      return ((), (s, t))

modifyFinishedGames ::
  (FinishedGames -> FinishedGames)
  -> Game ()
modifyFinishedGames f =
  Loop $ \_ (s, t) -> return ((), (s, f t))

finishedGames ::
  Game FinishedGames
finishedGames =
  Loop $ \_ (s, t) -> return (t, (s, t))

eGetLine ::
  Game String
eGetLine =
  initLoop (hGetLine . getL handleL)

ePutStrLn ::
  String
  -> Game ()
ePutStrLn s =
  initLoop (\env -> (hPutStrLn (handleL `getL` env) s))

allClients ::
  Game (Set Ref)
allClients =
  initLoop $ \env -> (readIORef (clientsL `getL` env))

process ::
  Command
  -> Game ()
process (Move p)    = error "todo"

process (Current)   = 
    withCurrentBoard "> empty" show >>= ePutStrLn >> return ()

process (Finished)  = (fmap . fmap) show finishedGames 
                        >>= ePutStrLn . intercalate "\n" 
                        >> return ()

process (Chat s)    = allClientsButThis ! ("> " ++ s)

process (Turn)      = do
    p <- withCurrentBoard Cross whoseTurn
    ePutStrLn $ show p
    return ()

process (At p)      = do
    maybeP <- withCurrentBoard Nothing (playerAt p)
    ePutStrLn $ maybe "> empty" (\x -> "> " ++ show x) maybeP
    return ()

process (Unknown s) = ePutStrLn $ "> Unknown command: " ++ s

game ::
  Game x -- client accepted (post)
  -> (String -> Game w) -- read line from client
  -> IO a
game g f = 
    iorefLoop ZeroMoves (ZeroMoves, []) g f

play ::
  IO a
play =
  game (currentUnfinished >>= pPutStrLn . show) (process . command)
