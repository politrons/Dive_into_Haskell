module HttpServer where

import Network
import Control.Concurrent
import System.IO

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

myServer = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock

loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       hPutStr h msg
       hFlush h
       hClose h
