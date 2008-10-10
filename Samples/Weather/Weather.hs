module Main where

--
-- Asynchronously downloads the Sydney weather forecast from the Australian
-- Bureau of Meterology FTP site (using a worker thread) and displays the
-- result.  A progress indicator is displayed during the download.
--

import Control.Concurrent.MVar
import Control.Monad
import System.IO

import Foreign.Salsa
import Bindings

main :: IO ()
main = withCLR $ do
    hSetBuffering stdout NoBuffering

    -- Make an MVar to hold the result of the download when it has completed
    done <- newEmptyMVar 

    -- Create a delegate that accepts a single argument and puts it into the
    -- MVar when called
    finished <- delegate _ParameterizedThreadStart (putMVar done)

    -- Instantiate a WebClient for downloading the weather forecast, and set
    -- the DownloadStringCompleted event to the @downloaded@ function.
    client <- new _WebClient ()
    set client [ _DownloadStringCompleted :+> delegate _DownloadStringCompletedEventHandler downloaded ]

    -- Download the weather forecast asynchronously, using the @finished@
    -- delegate as user state for the handler.
    uri <- new _Uri ("ftp://ftp2.bom.gov.au/anon/gen/fwo/IDN10064.txt")
    client # _downloadStringAsync (uri, cast finished :: Obj Object_)

    putStr "Downloading... |"

    -- Display an animated progress indicator until the MVar is filled
    let wait (c:cs) = do putStr ('\b':[c])
                         _Thread # _sleep (250 :: Int32)
                         isNotDone <- isEmptyMVar done
                         when isNotDone $ wait (cs ++ [c])
                         putStr "\b \n"
    wait "/-\\|"

    -- Display the result from the MVar
    s <- takeMVar done
    s # _toString () >>= putStrLn

downloaded :: Obj Object_ -> Obj DownloadStringCompletedEventArgs_ -> IO ()
downloaded _ e = do
    -- Get the result object (it will be a .NET string instance), and the
    -- finished object delegate (it will be a .NET delegate instance)
    result   <- get e _Result
    finished <- get e _UserState 

    -- Invoke the finished delegate, passing it the result
    (cast $ finished :: Obj ParameterizedThreadStart_) # _invoke result
    
-- vim:set sw=4 ts=4 expandtab:
