import Data.List
import System.Exit
import Network.Socket
import Network.BSD
import Text.Printf
import Data.Char (toLower)

import System.IO
import System.Time

import Control.Arrow
import Control.Monad.Reader
import Control.Exception

server = "irc.freenode.org"
port = "6667"
chan = "#y32"
nick = "hnng"

data Bot = Bot { socket :: Handle, startTime :: ClockTime }
type Net = ReaderT Bot IO

main :: IO()
main = bracket Main.connect disconnect loop
  where
    disconnect = hClose . Main.socket
    loop st = runReaderT run st

connect :: IO Bot
connect = notify $ do
  clock <- getClockTime
  proto <- getProtocolNumber "tcp"
  addrsInfo <- getAddrInfo Nothing (Just server) (Just port)
  let host = head addrsInfo

  printf "\nConnection Starting: %s : %s\n" server port

  sock <- Network.Socket.socket (addrFamily host) Stream proto
  Network.Socket.connect sock (addrAddress host)
  printf "Connected.\n"
  handle <- socketToHandle sock ReadWriteMode

  hSetBuffering handle NoBuffering
  return (Bot handle clock)
 where
   notify a = bracket_
     (printf "Connecting to %s..." server >> hFlush stdout)
     (putStrLn "Done.")
     a

run :: Net()
run = do
  write "NICK" nick
  write "USER" (nick ++ " 0 * :hnng")
  write "JOIN" chan
  asks Main.socket >>= Main.listen

write :: String -> String -> Net()
write s t = do
  h <- asks Main.socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO

eval :: String -> Net()
eval "!quit" = write "QUIT" ":Heck off" >> io (exitWith ExitSuccess)
eval "!uptime" = uptime >>= privmsg
eval x | "!say " `isPrefixOf` x = privmsg (drop 5 x)
eval x | "!test" `isPrefixOf` x = privmsg x
eval x | "!about" `isPrefixOf` x = privmsg "Haskell Botto by PixaL kthxbye"
eval x | x `containsIgnoreCase` "yeet" = privmsg "YEET"
eval x | x `containsIgnoreCase` "thanos car" = privmsg "THANOS CAR"
eval x | x `containsIgnoreCase` "COCKS" = privmsg "DICKS."
eval x | x `containsIgnoreCase` "gay" = privmsg "gay"
-- makeshift "on join event"
-- eval x | ("JOIN " ++ chan) `isInfixOf` x = privmsg "YEET."
eval _ = return ()

privmsg :: String -> Net()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

containsIgnoreCase :: String -> String -> Bool
containsIgnoreCase s word = isInfixOf (map toLower word) (map toLower s)

listen :: Handle -> Net()
listen h = forever $ do
  s <- init `fmap` io (hGetLine h)
  io $ putStrLn s
  if ping s then pong s else eval (clean s)
 where
   forever a = a >> forever a
   clean = drop 1 . dropWhile(/= ':') . drop 1
   ping x = "PING :" `isPrefixOf` x
   pong x = write "PONG" (':' : drop 6 x)

uptime :: Net String
uptime = do
  now <- io getClockTime
  zero <- asks startTime
  return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td =
  unwords $ map (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (tdSec td,[]) metrics

