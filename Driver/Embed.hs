-----------------------------------------------------------------------------
-- |
-- Program     : Embed
-- Copyright   : (c) 2007-2008 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE)
-- 
-- Generates a Haskell module that contains the binary data of the Salsa
-- driver assembly (typically, Salsa.dll) as an (unboxed) string literal.  This
-- is used to embed the driver assembly in every program that uses Salsa.
--
-----------------------------------------------------------------------------
module Main where

import System.Environment
import qualified Data.ByteString as S
import Text.Printf
import Data.Time.LocalTime (getZonedTime)

main :: IO ()
main = do
    now <- getZonedTime
    printf   "-- Generated at %s" now
    putStrLn "module Foreign.Salsa.Driver (driverData) where"
    putStrLn "import qualified Data.ByteString.Char8 as B"
    putStrLn "{-# NOINLINE driverData #-}"
    putStr "driverData = B.pack \""
    [inputFile] <- getArgs
    s <- S.readFile inputFile
    mapM_ (\x -> printf "\\x%02x" x) (S.unpack s)
    putStrLn "\""

-- vim:set ts=4 sw=4 expandtab:
