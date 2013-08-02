-- http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

import Network
import System.IO
import Text.Printf

server = "hobana.freenode.net"
port = 6667
chan = "#tutbot-testing"
nick = "tutbot"

main = do
  handle <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering handle NoBuffering
  write handle "NICK" nick
  write handle "USER" (nick ++ " 0 * :tutorial bot")
  write handle "JOIN" chan
  listen handle
  
write :: Handle -> String -> String -> IO ()  
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf "> %s %s\n" s t
  
listen :: Handle -> IO ()
listen h = forever $ do
  s <- hGetLine h
  putStrLn s
  where
    forever a = do a ; forever a