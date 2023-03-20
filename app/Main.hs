module Main where

import Network.Socket
import System.IO
import Control.Concurrent

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0             -- Create socket
    setSocketOption sock ReuseAddr 1            -- Make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 0)             -- Listen on TCP port 4242.
    listen sock 2                               -- Allow a maximum of 2 queued connections
    mainLoop sock


mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock                        -- Accept a connection and handle it
    forkIO $ runConn conn                      -- Fork to a new thread to handle the connection
    mainLoop sock


-- Handle a single connection
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode    -- Convert socket to a handle
    hPutStrLn hdl "Hello world!"
    hClose hdl