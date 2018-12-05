module MessageSystem where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)


data Message = Message {msgId :: Int, msg ::String}

messageSystem :: IO ()
messageSystem = do sock <- socket AF_INET Stream 0
                   channel <- initServer sock
                   mainLoop sock channel 0

{-|
  Init function to configure the socket following the next steps:
   * Create a new channel for the communications.
   * Bind the socket to listen in the port configured.
   * Invoke [newChan] function to sets up the read and write end of a channel by initialising [Chan] with
     empty @MVar@s for read and empty @MVar@ for write
-}
initServer:: Socket -> IO (Chan Message)
initServer sock =  do channel <- newChan
                      setSocketOption sock ReuseAddr 1
                      bind sock (SockAddrInet 1981 iNADDR_ANY)
                      listen sock 2
                      return channel

{-| [accept] function create a socket connection, then [forkIO] creates a new thread
  to run the 'IO' computation passed as the first argument, and then make a recursive call in a infinite loop -}
mainLoop :: Socket -> Chan Message -> Int -> IO ()
mainLoop sock channel msgId = do conn <- accept sock
                                 forkIO $ runConn conn channel msgId
                                 mainLoop sock channel (msgId + 1)

{-| We use [writeChan] function to put an element on a channel and this will give us a
    [broadcast] which it will be used for the communication between clients.
    The function [socketToHandle] transform a [sock] into a [handle]
 -}
runConn :: (Socket, SockAddr) -> Chan Message -> Int -> IO ()
runConn (sock, _) channel msgId = do
                                let broadcast msg = writeChan channel $ Message msgId msg
                                handle <- createHandle sock ReadWriteMode
                                name <- logInClient handle broadcast
                                newChannel <- duplicateChannel channel
                                readerThreadId <- readMessage handle newChannel msgId
                                writeMessage broadcast handle name readerThreadId
                                hClose handle

{-| Function which passing the [socket] and the [IOMode] we're able to create the handle, which is used to
    interact with the client terminal for input/output of data-}
createHandle :: Socket -> IOMode -> IO Handle
createHandle socket ioMode = do handle <- socketToHandle socket ioMode
                                setBufferMode handle NoBuffering
                                return handle


{-| Set the Buffering Operations when we read/write into the client terminal-}
setBufferMode ::Handle -> BufferMode -> IO()
setBufferMode handler bufferMode = hSetBuffering handler bufferMode

{-| Function to add new client into the system, and inform the current clients about the new one.
   [printTerminalMsg] together with [handle] and the message is used to print the message in the terminal of the client.
    Then we use [hGetLine] together with [handle] to get the input name of the client-}
logInClient :: Handle -> (String -> IO()) -> IO [Char]
logInClient handle broadcast =  do printClientMsg handle "Welcome to Politrons message system. How shall I call you?"
                                   name <- fmap init (hGetLine handle)
                                   broadcast ("Human " ++ name ++ " has join into the system.")
                                   printClientMsg handle ("Human " ++ name ++ " logged")
                                   return name

{-| Duplicate a 'Chan': the duplicate channel begins empty, but data written to
   either channel from then on will be available from both.  Hence this creates
   a kind of broadcast channel, where data written by anyone is seen by
   everyone else. -}
duplicateChannel :: Chan Message -> IO (Chan Message)
duplicateChannel channel = dupChan channel

{-| [forkIO] fork off a thread for reading from the duplicated channel so the pipeline can continue the execution. -}
readMessage :: Handle -> Chan Message -> Int -> IO ThreadId
readMessage handle channel msgNum = do threadId <- forkIO $ readMessagesForever handle channel msgNum
                                       return threadId

{-|
  [fix] as the documentation describe is used to do recursive operations as anonymous function defined at the beginning
  in this case [loop] is this anonymous function which is invoked forever.
  The [forkIO] return a processId/threadId which it can be used to destroy once the client disconnect. -}
readMessagesForever:: Handle -> Chan Message -> Int -> IO()
readMessagesForever handle channel msgNum = fix $ \loop -> do message <- readChan channel
                                                              when (msgNum /= (msgId message)) $ printClientMsg handle (msg message)
                                                              loop

{-| Again, using [fix] function we can use this recursive call to search in every iteration if there's a new input client
    message, and if it does and is not the [exit] command, we broadcast to the rest of the clients.
    With [>>] operator which is an applicative we can compose functions in one line. As here we broadcast and call the recursive function loop-}
writeMessage:: (String -> IO()) -> Handle -> [Char] -> ThreadId -> IO()
writeMessage broadcast handle name readerId = fix $ \loop -> do message <- fmap init (hGetLine handle)
                                                                case message of
                                                                 "exit" -> logOutClient broadcast handle name readerId
                                                                 _      -> broadcast (name ++ ": " ++ message) >> loop

{-| Function to logoff a client, where we do the next composition of functions:
    * Print in the terminal of the client that has been logged off,
    * broadcast all client that the client X has logged off
    * Kill the current process loop of reading message from the channel.
-}
logOutClient :: (String -> IO()) -> Handle -> [Char] -> ThreadId -> IO()
logOutClient broadcast handle name readerId = printClientMsg handle (name ++ "logged off")
                                           >> broadcast (name ++ ": Has logged off ")
                                           >> killThread readerId

{-| Function to print in the client a message. Every client has a handle associated, which it was created through the channel.
  [hPutStrLn] function is used to print in the client terminal the message received.
-}
printClientMsg :: Handle -> [Char] -> IO()
printClientMsg handle msg = hPutStrLn handle msg