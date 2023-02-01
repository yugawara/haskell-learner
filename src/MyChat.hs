module MyChat where
import Network.Socket
import System.IO

main9 :: IO ()
main9 = withSocketsDo $ do
    -- Create a socket
    sock <- socket AF_INET Stream 0
    -- Connect to the specified host and port
    connect sock (SockAddrInet 1234 (tupleToHostAddress (127,0,0,1)))
    -- Get a handle to the socket
    hdl <- socketToHandle sock ReadWriteMode
    -- Send a message
    hPutStrLn hdl "Hello, World!"
    -- Close the handle
    hClose hdl
