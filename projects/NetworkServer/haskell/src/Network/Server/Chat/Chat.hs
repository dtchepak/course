module Network.Server.Chat.Chat where

import Network.Server.Common.Env
import Network.Server.Common.Line
import Network.Server.Chat.Loop
import Data.IORef (atomicModifyIORef)
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Control.Arrow ((&&&))
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))

type Chat a =
  IORefLoop Integer a

data ChatCommand =
  Chat String
  | Incr
  | Unknown String
  deriving (Eq, Show)

incr ::
  Chat Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef e (succ &&& succ)

chat ::
  IO a
chat =
  iorefLoop 0 (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand ::
  String
  -> ChatCommand
chatCommand z =
  Unknown z `fromMaybe` msum [
                               Chat <$> trimPrefixThen "CHAT" z
                             , Incr <$ trimPrefixThen "INCR" z
                             ]

serverMsg :: String -> String
serverMsg = (++) "> "

process ::
  ChatCommand
  -> Chat ()
process (Chat s) = allClientsButThis ! serverMsg s
process (Unknown s) = pPutStrLn . serverMsg $ "Don't know how to " ++ s
process Incr = 
    let msg n = serverMsg $ "counter is at " ++ show n
    in incr >>= \n -> allClients ! msg n
