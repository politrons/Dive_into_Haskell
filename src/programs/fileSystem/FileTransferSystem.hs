module FileTransferSystem where

import           Control.Concurrent
import           Control.Exception
import           Control.Exception              (SomeException, evaluate, try)
import           Control.Monad                  (when)
import           Control.Monad.Fix              (fix)
import           Data.ByteString                (pack, unpack)
import qualified Data.ByteString.Lazy           as BS
import           Data.ByteString.Lazy.Char8     (ByteString)
import qualified Data.ByteString.Lazy.Char8     as C
import           Data.Char                      (toUpper)
import           Data.List                      (isInfixOf)
import           Network.HTTP.Client
import           Network.HTTP.Client            (Response)
import           Network.HTTP.Types.Status      (statusCode)
import           Network.Socket                 hiding (getContents, recv)
import           Network.Socket.ByteString.Lazy (getContents, recv, sendAll)
import           System.IO
import           System.IO

type MsgId = Int

data Message = Message
  { msgId :: Int
  , msg   :: String
  }

{-| Util Function that make composition of main functions in case you want to run the server, sender and consumer in the same
    haskell program.-}
mainFileTransferProgram :: IO ()
mainFileTransferProgram = do
  _ <- forkIO fileSystem
  threadDelay 2000000
  _ <- forkIO receiverClient
  threadDelay 2000000
  _ <- senderClient "somefile.txt"
  threadDelay 2000000
  print "done"

{-| ----------------------------------------------}
{-|                    CLIENTS                   -}
{-| ----------------------------------------------}
{-| Client logic which contains two clients. [sender] which send the files. And [receiver] which are connected
    to the system waiting to receive data-}
{-| Function in charge to open a socket to the server, read the local file specify in [filePath] argument
    and send to the server the data-}
senderClient :: String -> IO ()
senderClient filePath = do
  sock <- openConnection
  fileContent <- readFileToSend filePath
  sendAll sock $ C.pack fileContent
  close sock

{-| Function in charge to open a socket to the server, open a handle specifying a file name in [WriteMode],
    invoke recursive function [listeningMessage] to scan the socket for data.
    Once return from previous function we close the handle to write on disk and we close the socket-}
receiverClient :: IO ()
receiverClient = do
  sock <- openConnection
  handle <- openFile "fileOutput.txt" WriteMode
  listeningMessage (C.pack "") handle sock
  hClose handle
  print "File transfer end......"
  close sock

{-| Create a socket Open a connection in the socket against an ip/port and return the socket opened.-}
openConnection :: IO Socket
openConnection = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2981")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock

{-| Function responsible to read from the socket new data using [recv] function.
    Since network is impure and it's possible side effects we control the effect of network with [try/evaluate] error handling
    Once we receive data recv function return the control of the program and pass a chunk of data, which is used by [handle] to write in disk.
    Since [recv] cannot guarantee the recipe of all data, we need to use [recursive] calls until the condition [endOfSocket] happans.-}
listeningMessage :: ByteString -> Handle -> Socket -> IO ()
listeningMessage totalMsg handle sock = do
  eitherResult <- try (evaluate (recv sock 1024)) :: IO (Either SomeException (IO ByteString))
  case eitherResult of
    Left ex -> do
      print $ "Socket error " ++ show ex
      hPutStrLn handle (C.unpack totalMsg)
    Right ioMsg -> do
      msg <- ioMsg
      let endOfSocket = isInfixOf "\n" (C.unpack msg) -- Not a good filter!
      if endOfSocket
        then do
          print "End of reading"
          hPutStrLn handle (C.unpack (totalMsg <> msg))
        else listeningMessage (totalMsg <> msg) handle sock

{-| Function responsible to read the local file to and transform into [Char]-}
readFileToSend :: String -> IO String
readFileToSend = readFile

{-| ----------------------------------------------}
{-|                     SERVICE                  -}
{-| ----------------------------------------------}
{-| Server logic responsible to receive data and transmit by broadcast to all user connected to the system-}
fileSystem :: IO ()
fileSystem = do
  sock <- socket AF_INET Stream 0
  channel <- createChannel sock
  print "Starting File transfer server at port 2981"
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
createChannel :: Socket -> IO (Chan Message)
createChannel sock = do
  channel <- newChan
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 2981 iNADDR_ANY)
  listen sock 2
  return channel

{-| [accept] function create a socket connection when a new connection from a client is open.
    [forkIO] creates a new thread  to run the 'IO' computation passed as the first argument.
    Finally make a recursive call to block the program in [accept] waiting for a new connection. -}
acceptConnectionsLoop :: Socket -> Chan Message -> MsgId -> IO ()
acceptConnectionsLoop sock channel msgId = do
  conn <- accept sock
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
createHandle socket ioMode = do
  handle <- socketToHandle socket ioMode
  setBufferMode handle NoBuffering
  return handle

{-| Set the Buffering Operations when we read/write into the client terminal
    Thanks to eta reduce we can avoid write the function argument -}
setBufferMode :: Handle -> BufferMode -> IO ()
setBufferMode = hSetBuffering

{-| Duplicate a 'Channel': the duplicate channel begins empty, but data written to
   either channel from then on will be available from both.  Hence this creates
   a kind of broadcast channel, where data written by anyone is seen by  everyone else.
   Thanks to eta reduce we can avoid write the function argument [channel] when we use [dupChan] function-}
duplicateChannel :: Chan Message -> IO (Chan Message)
duplicateChannel = dupChan

{-| [forkIO] fork off a thread for reading from the duplicated channel so the pipeline can continue the execution.
   The [forkIO] return a processId/threadId which it can be used to destroy once the client disconnect. -}
readMessage :: Handle -> Chan Message -> Int -> IO ThreadId
readMessage handle channel msgNum = forkIO $ readMessagesForever handle channel msgNum

{-|
  [fix] function as the documentation describe is used to do recursive operations as anonymous function defined at the beginning
        in this case [loop] is this anonymous function which is invoked forever.
  [readChan] function read the next value from the 'Chan'. Blocks when the channel is empty. Since
             threads blocked in this operation are woken up in FIFO order).
  [when] function execute the operation passed in the parenthesis, and in case is true execute the next operation passed.
-}
readMessagesForever :: Handle -> Chan Message -> Int -> IO ()
readMessagesForever handle channel msgNum =
  fix $ \loop -> do
    message <- readChan channel
    when (msgNum /= msgId message) $ printClientMsg handle (msg message)
    loop

{-| Again, using [fix] function we can use this recursive call to search in every iteration if there's a new input client
    message, and if it does and is not the [exit] command, we broadcast to the rest of the clients.
    With [>>] operator which is an applicative we can compose functions in one line. As here we broadcast and call the recursive function loop-}
writeMessage :: (String -> IO ()) -> Handle -> ThreadId -> IO ()
writeMessage broadcast handle readerId =
  fix $ \loop -> do
    message <- readClientInput handle
    broadcast message >> loop

{-| Function that using [hGetLine] read from the client handle the input that has been typed-}
readClientInput :: Handle -> IO String
readClientInput handle = fmap init (hGetLine handle)

{-| Function to print in the client a message. Every client has a handle associated, which it was created through the channel.
  [hPutStrLn] function is used to print in the client terminal the message received.
-}
printClientMsg :: Handle -> String -> IO ()
printClientMsg = hPutStrLn
