import Data.List
import Data.List.Split
import System.Exit
import Network.Socket
import Text.Printf
import Data.Char (toLower)

import System.IO
import Data.IORef
import Data.Time
import Data.Time.Clock

import Control.Arrow
import Control.Monad.Reader
import Control.Exception

server = "irc.freenode.org"
port = "6667"
nick = "hnng"

channels = ["#y32", "#ar1a"]

data Bot = Bot { socket :: Handle, startTime :: UTCTime }

type Net = ReaderT Bot IO

main :: IO()
main = bracket Main.connect disconnect loop
  where
    disconnect = hClose . Main.socket
    loop = runReaderT run

connect :: IO Bot
connect = notify $ do
  clock <- getCurrentTime
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just server) (Just port)

  printf "\nConnection Starting: %s : %s\n" server port

  sock <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Network.Socket.connect sock (addrAddress addr)
  printf "Connected.\n"
  handle <- socketToHandle sock ReadWriteMode

  hSetBuffering handle NoBuffering
  return (Bot handle clock)
 where
   notify = bracket_
     (printf "Connecting to %s..." server >> hFlush stdout)
     (putStrLn "Done.")

run :: Net()
run = do
  write "NICK" nick
  write "USER" (nick ++ " 0 * :hnng")
  forM_ channels $ \channel -> write "JOIN" channel
  asks Main.socket >>= Main.listen

write :: String -> String -> Net()
write s t = do
  h <- asks Main.socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO

eval :: [String] -> Net()
eval [] = return ()
eval (_:[]) = return ()
eval (nick:channel:text:[])
  | "!quit" `isPrefixOf` text = quit nick channel
  | "!uptime" `isPrefixOf` text = uptime >>= privmsg channel
  | "!say " `isPrefixOf` text = privmsg channel (drop 5 text)
  | "!test" `isPrefixOf` text = privmsg channel text
  | "!about" `isPrefixOf` text = privmsg channel "Haskell Botto by PixaL kthxbye"
  | text `containsIgnoreCase` "yeet" = privmsg channel "YEET"
  | text `containsIgnoreCase` "nigga toilet" = privmsg channel "NIGGA TOILET"
  | text `containsIgnoreCase` "thanos car" = privmsg channel "THANOS CAR"
  | text `containsIgnoreCase` "COCKS" = privmsg channel "DICKS."
  | text `containsIgnoreCase` "gay" = privmsg channel "gay"
  | otherwise = return ()
eval (_:_:_) = return ()
-- makeshift "on join event"
-- eval x | ("JOIN " ++ chan) `isInfixOf` x = privmsg "YEET."

quit :: String -> String -> Net()
quit n c
  | n == "PixeLInc"    = write "QUIT" ":Heck off" >> io exitSuccess
  | n == "godlyelixir" = privmsg c "pike pike pike"
  | otherwise          = privmsg c "Fuck off retard"

privmsg :: String -> String -> Net()
privmsg c s = write "PRIVMSG" ("#" ++ c ++ " :" ++ s)

containsIgnoreCase :: String -> String -> Bool
containsIgnoreCase s word = map toLower word `isInfixOf` map toLower s

listen :: Handle -> Net()
listen h = forever $ do
  s <- init `fmap` io (hGetLine h)
  io $ putStrLn s
  if ping s then pong s else eval (nick s : splitOn " :" (clean s))
 where
   forever a = a >> forever a
   nick s = drop 1 (head(splitOn "!~" s))
   clean = drop 1 . dropWhile(/= '#') . drop 1
   ping x = "PING :" `isPrefixOf` x
   pong x = write "PONG" (':' : drop 6 x)

uptime :: Net String
uptime = do
  now <- io getCurrentTime
  zero <- asks startTime
  return . pretty $ diffUTCTime now zero

pretty :: NominalDiffTime -> String
pretty td =
  unwords $ map (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
          foldl' merge (round (realToFrac td),[]) metrics

