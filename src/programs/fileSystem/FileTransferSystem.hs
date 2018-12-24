module FileTransferSystem where

--import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import System.IO
import Data.Char(toUpper)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString (pack,unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Exception (SomeException,try,evaluate)
import Network.HTTP.Client (Response)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)


type MsgId = Int
data Message = Message {msgId :: Int, msg ::String}

mainFileTransferProgram :: IO()
mainFileTransferProgram = do _ <- forkIO fileSystem
                             threadDelay 2000000
                             _ <- forkIO receiveFile
                             threadDelay 2000000
                             _ <- sendFile "somefile.txt"
                             threadDelay 10000000
                             print "done"

{-| ----------------------------------------------}
{-|                    CLIENT                    -}
{-| ----------------------------------------------}
sendFile :: [Char] -> IO()
sendFile filePath = do sock <- openConnection
                       fileContent <- readFileToSend filePath
                       sendAll sock $ C.pack fileContent
                       close sock

receiveFile :: IO()
receiveFile = do sock <- openConnection
                 handle <- openFile "fileOutput.txt" WriteMode
                 forkIO $ listeningMessage handle sock
                 threadDelay 5000000
                 hClose handle
                 print "File transfer end......"
                 close sock

openConnection :: IO Socket
openConnection = do addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2981")
                    let serveraddr = head addrinfos
                    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                    connect sock (addrAddress serveraddr)
                    return sock

listeningMessage::Handle -> Socket -> IO()
listeningMessage handle sock = fix $ \loop -> do msg <- recv sock 4096
                                                 print $ "INPUT DATA:" ++ show msg
                                                 hPutStrLn handle (C.unpack msg)
                                                 loop

readFileToSend :: [Char] -> IO [Char]
readFileToSend filePath = do fileContent <- readFile filePath
                             return fileContent

{-| ----------------------------------------------}
{-|                     SERVICE                  -}
{-| ----------------------------------------------}

fileSystem :: IO ()
fileSystem = do sock <- socket AF_INET Stream 0
                channel <- createChannel sock
                print ("Starting File transfer server at port 2981")
                acceptConnectionsLoop sock channel 0

{-|
  Init function to configure the socket following the next steps:
   * Create a new channel for the communications.
   * Bind the socket to listen in the port configured.
   * Set max number of queued connections. Mandatory
   * Invoke [newChan] function to sets up the read and write end of a channel by initialising [Chan] with
     empty @MVar@s for read and empty @MVar@ for write
     Thanks to haskell infer types, [newChan] know that [Chan a] is the same than the one we return in the function
     [Chan Message]
-}
createChannel:: Socket -> IO (Chan Message)
createChannel sock =  do channel <- newChan
                         setSocketOption sock ReuseAddr 1
                         bind sock (SockAddrInet 2981 iNADDR_ANY)
                         listen sock 2
                         return channel

{-| [accept] function create a socket connection when a new connection from a client is open.
    [forkIO] creates a new thread  to run the 'IO' computation passed as the first argument.
    Finally make a recursive call to block the program in [accept] waiting for a new connection. -}
acceptConnectionsLoop :: Socket -> Chan Message -> MsgId -> IO ()
acceptConnectionsLoop sock channel msgId = do conn <- accept sock
                                              forkIO $ processClientConnection conn channel msgId
                                              acceptConnectionsLoop sock channel (msgId + 1)

{-| We use [writeChan] function together with a channel and Message type to define a high order function [broadcast]
   which it will be used for the communication between clients.
   The function [createHandle] transform a [sock] into a [handle] to communicate directly with the owner of that socket -}
processClientConnection :: (Socket, SockAddr) -> Chan Message -> MsgId -> IO ()
processClientConnection (sock, _) channel msgId = do
                                let broadcast msg = writeChan channel $ Message msgId msg
                                handle <- createHandle sock ReadWriteMode
                                newChannel <- duplicateChannel channel
                                readerThreadId <- readMessage handle newChannel msgId
                                writeMessage broadcast handle readerThreadId

{-| Function which passing the [socket] and the [IOMode] we're able to create the handle, which is used to
    interact with the client terminal for input/output of data-}
createHandle :: Socket -> IOMode -> IO Handle
createHandle socket ioMode = do handle <- socketToHandle socket ioMode
                                setBufferMode handle NoBuffering
                                return handle

{-| Set the Buffering Operations when we read/write into the client terminal-}
setBufferMode ::Handle -> BufferMode -> IO()
setBufferMode handler bufferMode = hSetBuffering handler bufferMode


{-| Duplicate a 'Channel': the duplicate channel begins empty, but data written to
   either channel from then on will be available from both.  Hence this creates
   a kind of broadcast channel, where data written by anyone is seen by
   everyone else. -}
duplicateChannel :: Chan Message -> IO (Chan Message)
duplicateChannel channel = dupChan channel

{-| [forkIO] fork off a thread for reading from the duplicated channel so the pipeline can continue the execution.
   The [forkIO] return a processId/threadId which it can be used to destroy once the client disconnect. -}
readMessage :: Handle -> Chan Message -> Int -> IO ThreadId
readMessage handle channel msgNum = do threadId <- forkIO $ readMessagesForever handle channel msgNum
                                       return threadId

{-|
  [fix] function as the documentation describe is used to do recursive operations as anonymous function defined at the beginning
        in this case [loop] is this anonymous function which is invoked forever.
  [readChan] function read the next value from the 'Chan'. Blocks when the channel is empty. Since
             threads blocked in this operation are woken up in FIFO order).
  [when] function execute the operation passed in the parenthesis, and in case is true execute the next operation passed.
-}
readMessagesForever:: Handle -> Chan Message -> Int -> IO()
readMessagesForever handle channel msgNum = fix $ \loop -> do message <- readChan channel
                                                              when (msgNum /= (msgId message)) $ printClientMsg handle (msg message)
                                                              loop

{-| Again, using [fix] function we can use this recursive call to search in every iteration if there's a new input client
    message, and if it does and is not the [exit] command, we broadcast to the rest of the clients.
    With [>>] operator which is an applicative we can compose functions in one line. As here we broadcast and call the recursive function loop-}
writeMessage:: (String -> IO()) -> Handle -> ThreadId -> IO()
writeMessage broadcast handle readerId = fix $ \loop -> do message <- readClientInput handle
                                                           broadcast (message) >> loop

{-| Function that using [hGetLine] read from the client handle the input that has been typed-}
readClientInput::Handle -> IO [Char]
readClientInput handle = fmap init (hGetLine handle)

{-| Function to print in the client a message. Every client has a handle associated, which it was created through the channel.
  [hPutStrLn] function is used to print in the client terminal the message received.
-}
printClientMsg :: Handle -> [Char] -> IO()
printClientMsg handle msg = hPutStrLn handle msg
