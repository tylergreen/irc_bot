import Prelude hiding (catch)
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf

server = "hobana.freenode.net"
port = 6667
chan = "#tutbot-testing"
nick = "tutbot"

data Bot = Bot { socket :: Handle, starttime :: UTCTime }

-- combine Bot and IO types into a Net type
-- ReaderT is a type constructor
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
       where 
         disconnect = hClose . socket    
         loop st = catch (runReaderT run st) catchException
         
catchException e = do let err = show (e :: IOException) 
                      hPutStr stderr ("IO Exception!!")
                      return ()

connect :: IO Bot
connect = notify $ do
  time <- getCurrentTime
  handle <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering handle NoBuffering
  return (Bot handle time)
   
notify a = bracket_
           (printf "Connecting to %s ... " server >> hFlush stdout)
           (putStrLn "done.")
           a
          
run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick ++ " 0 * :tutorial bot")
  write "JOIN" chan
  asks socket >>= listen
  
listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` io (hGetLine h)
  io (putStrLn s)
  if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" ( ':' : drop 6 x)
    
write :: String -> String -> Net ()  
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t
                               
writeAll :: String -> [String] -> Net ()  
writeAll s ts = do
  h <- asks socket
  io $ writeAll_ h (s:ts)
  io $ printf "> %s %s\n" s (unwords ts)
  
writeAll_ :: Handle -> [String] -> IO ()
writeAll_ h xs = mapM_ (\x -> hPrintf h "%s " x) xs >> hPrintf h "\r\n"

io :: IO a -> Net a
io = liftIO

eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval "!uptime"  = uptime >>= privmsg 
eval x | "!id" `isPrefixOf` x = privmsg $ drop 4 $ x
eval _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

uptime :: Net String
uptime = do
  now <- io getCurrentTime
  zero <- asks starttime
  return $ show $ diffUTCTime now zero
  







  