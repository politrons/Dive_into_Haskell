module HttpServer where

import Network
import Control.Concurrent
import System.IO

responseMessage = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
port = 1981 :: PortNumber

myServer = withSocketsDo $ do
    socket <- createSocket port
    print ("Server running in port " ++ show port)
    service socket

service :: Socket -> IO()
service socket = do
   (handler,hostname,portNumber) <- accept socket --  Accept a connection on a socket created by 'listenOn'
   print ("Request received by hostname " ++ hostname)
   forkIO $ createResponse handler -- Running response in a new thread
   service socket --Recursive call to subscribe again for new request

createSocket :: PortNumber -> IO Socket
createSocket port = listenOn $ PortNumber port -- $ means like use parenthesis listenOn(PortNumber 5002)

createResponse :: Handle -> IO ()
createResponse handler = do
                         hPutStr handler responseMessage -- Write the response in output buffer.
                         hFlush handler -- Sent data in buffer immediately to the operating system.
                         hClose handler -- Close handler.