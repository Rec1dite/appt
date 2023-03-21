module Main where

-- Sockets + Concurrency
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix
import Control.Monad (unless)

-- Appointment Data
import Data.UnixTime
import Data.ByteString.UTF8 (toString, fromString)
import Crypto.Hash.SHA256

-- File IO
import System.Directory
import System.FilePath.Posix
import Data.List (findIndex)


type Msg = String


----------------- IO -----------------
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0             -- Create socket
    setSocketOption sock ReuseAddr 1            -- Make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4000 0)             -- Listen on TCP port 4000
    listen sock 2                               -- Allow a maximum of 2 queued connections

    chan <- newChan                             -- Create a new message channel
    mainLoop sock chan

-- The server loop which listens for new connections
mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    conn <- accept sock                        -- Accept a connection and handle it
    forkIO $ runConn conn                      -- Fork to a new thread to handle the connection
    mainLoop sock chan

-- Handle a single connection
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode    -- Convert socket to a handle
    hSetBuffering hdl NoBuffering               -- Set buffering mode

    welcome hdl
    -- authenticate hdl
    runSession hdl -- Loops

    hClose hdl


runSession :: Handle -> IO ()
runSession hdl = do
    input <- hGetLine hdl

    unless (take 4 input == "quit") $ do
        -- Split input into args
        let args = words input

        -- Handle command
        let cmd = lookup (head args) dispatch
        case cmd of
            Just f -> f hdl args
            Nothing -> help hdl args

        -- Loop
        runSession hdl


----------------- COMMANDS -----------------

authenticate :: Handle -> IO ()
authenticate hdl = do
    hPutStrLn hdl "Please enter your name: "
    name <- hGetLine hdl
    hPutStrLn hdl $ "Welcome, " ++ name ++ "!"

welcome :: Handle -> IO ()
welcome = flip hPutStrLn "\ESC[31mWelcome!\ESC[0m\n";

dispatch :: [(String, Handle -> [String] -> IO ())]
dispatch = [ ("help\r\n", help)
           , ("list\r\n", list)
           , ("search\r\n", search)
           , ("clear\r\n", clear)
           , ("add\r\n", add)
           , ("remove\r\n", remove)
           ]

help :: Handle -> [String] -> IO ()
help hdl _ = hPutStrLn hdl
  "\nAppointments Manager\n\
  \-----------------------\n\
  \\ESC[1;34m\
  \help         - show this message\n\
  \search <arg> - search appointments\n\
  \list         - list all appointments\n\
  \clear        - clear the screen\n\
  \add <args>   - add a new appointment\n\
  \remove <n>   - remove appoint with id n\n\
  \\ESC[0m"

search :: Handle -> [String] -> IO ()
search hdl _ = do
    appts <- getAppointments
    return ()

clear :: Handle -> [String] -> IO ()
clear hdl _ = hPutStrLn hdl "\ESC[2J"

list :: Handle -> [String] -> IO ()
list hdl _ = do
    appts <- getAppointments
    hPutStrLn hdl "Your appointments:"
    hPrint hdl appts

add :: Handle -> [String] -> IO ()
add hdl _ = do
    hPutStrLn hdl "Enter time [hh:mm|dd/mm/yy]:"
    time <- hGetLine hdl
    hPutStrLn hdl "Enter location:"
    loc <- hGetLine hdl
    hPutStrLn hdl "Enter description:"
    desc <- hGetLine hdl

    case createAppointment [time, loc, desc] of
        Nothing -> hPutStrLn hdl "Invalid appointment"
        Just appointment -> do
            let hash = hashAppointment appointment
            createAndWriteFile (appointDir ++ hash) (show appointment)
            hPutStrLn hdl $ "Appointment " ++ hash ++ " created"


createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
    createDirectoryIfMissing True (takeDirectory path)

remove :: Handle -> [String] -> IO ()
remove hdl _ = do
    hPutStrLn hdl "Enter start of appointment id:"
    id <- hGetLine hdl
    appts <- getAppointments
    let appt = findIndex (\a -> hashAppointment a == id) appts

    removeFile (appointDir ++ id)
    hPutStrLn hdl $ "Appointment " ++ id ++ " removed"


----------------- APPOINTMENTS -----------------
-- Add notes to appointments
-- Hash appointments for unique id --> No duplicate appointments
-- Store each appointment in its own file

data Appointment = Appointment { time :: UnixTime, location :: String, description :: String } deriving (Show)

appointDir :: FilePath
appointDir = "appointments/"

createAppointment :: [String] -> Maybe Appointment
createAppointment [time, loc, desc] = Just $ Appointment (parseUnixTime (fromString "%H:%M|%d/%M/%Y") (fromString time)) loc desc
createAppointment _ = Nothing

-- Hash an appointment to 16 char string
hashAppointment :: Appointment -> String
hashAppointment = take 16 . toString . hash . fromString . show

getAppointments :: IO [Appointment]
getAppointments = do
    files <- getDirectoryContents appointDir
    let appts = filter (`notElem` [".", ".."]) files
    mapM readFile appts