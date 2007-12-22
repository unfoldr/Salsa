{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Salsa.CLRHost
-- Copyright   : (c) 2007 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE.txt)
-- 
-- Exposes some of the methods of the ICLRRuntimeHost COM interface, which
-- can be used to host the the Microsoft CLR in the process, and to execute
-- code from a .NET assembly.  Includes basic functionality for dealing
-- with the Microsoft COM.
--
-----------------------------------------------------------------------------
module Salsa.CLRHost (
    corBindToRuntimeEx,
    start_ICLRRuntimeHost,
    stop_ICLRRuntimeHost,
    executeInDefaultAppDomain_ICLRRuntimeHost,
    ICLRRuntimeHost
    ) where

import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.String
import System.Win32
import System.IO

--
-- Global static functions for hosting the CLR
--

type ICLRRuntimeHost = InterfacePtr

-- | 'corBindToRunTimeEx' loads the CLR execution engine into the process and returns
--   a COM interface for it.
corBindToRuntimeEx :: IO ICLRRuntimeHost
corBindToRuntimeEx = do
    -- Load the 'mscoree' dynamic library into the process.  This is the
    -- 'stub' library for the .NET execution engine, and is used to load an
    -- appropriate version of the real runtime via a call to
    -- 'CorBindToRuntimeEx'.
    hMscoree <- loadLibrary "mscoree.dll"

    -- Obtain a pointer to the 'CorBindToRuntimeEx' function from mscoree.dll
    corBindToRuntimeExAddr <- getProcAddress hMscoree "CorBindToRuntimeEx"
    let corBindToRuntimeEx = makeCorBindToRuntimeEx $ castPtrToFunPtr corBindToRuntimeExAddr

    let clsid_CLRRuntimeHost = Guid 0x90F1A06E 0x7712 0x4762 0x86 0xB5 0x7A 0x5E 0xBA 0x6B 0xDB 0x02
        iid_ICLRRuntimeHost  = Guid 0x90F1A06C 0x7712 0x4762 0x86 0xB5 0x7A 0x5E 0xBA 0x6B 0xDB 0x02

    -- Request the shim (mscoree.dll) to load a version of the runtime into the
    -- process, returning a pointer to an implementation of the ICLRRuntimeHost
    -- for controlling the runtime.
    with (nullPtr :: ICLRRuntimeHost) $ \clrHostPtr -> do
        -- Call 'corBindToRuntimeEx' to obtain an ICLRRuntimeHost 
        hResult <- with clsid_CLRRuntimeHost $ \refCLSID_CLRRuntimeHost -> 
                       with iid_ICLRRuntimeHost $ \refIID_ICLRRuntimeHost -> 
                           corBindToRuntimeEx nullPtr nullPtr 0 refCLSID_CLRRuntimeHost
                               refIID_ICLRRuntimeHost clrHostPtr
        peek clrHostPtr

type CorBindToRuntimeEx = LPCWSTR -> LPCWSTR -> DWORD -> Ptr CLSID -> Ptr IID -> Ptr ICLRRuntimeHost -> IO HRESULT
foreign import stdcall "dynamic" makeCorBindToRuntimeEx :: FunPtr CorBindToRuntimeEx -> CorBindToRuntimeEx


-- | 'start_ICLRRuntimeHost' calls the Start method of the given ICLRRuntimeHost interface.
start_ICLRRuntimeHost this = do
    f <- getInterfaceFunction 3 makeStart this
    f this -- TODO: Check HRESULT, use error

type Start = ICLRRuntimeHost -> IO HRESULT
foreign import stdcall "dynamic" makeStart :: FunPtr Start -> Start


-- | 'stop_ICLRRuntimeHost' calls the Stop method of the given ICLRRuntimeHost interface.
stop_ICLRRuntimeHost this = do
    f <- getInterfaceFunction 4 makeStop this
    f this -- TODO: Check HRESULT, use error

type Stop = ICLRRuntimeHost -> IO HRESULT
foreign import stdcall "dynamic" makeStop :: FunPtr Stop -> Stop


-- | 'executeInDefaultAppDomain_ICLRRuntimeHost' calls the ExecuteInDefaultAppDomain
--   method of the given ICLRRuntimeHost interface.
executeInDefaultAppDomain_ICLRRuntimeHost :: ICLRRuntimeHost -> String -> String -> String -> String -> IO DWORD
executeInDefaultAppDomain_ICLRRuntimeHost this assemblyPath typeName methodName argument = do
    f <- getInterfaceFunction 11 makeExecuteInDefaultAppDomain this
    with (0 :: DWORD) $ \resultPtr -> do
        hResult <- withCWString assemblyPath $ \assemblyPath' -> 
                       withCWString typeName $ \typeName' ->
                           withCWString methodName $ \methodName' ->
                               withCWString argument $ \argument' ->
                                   f this assemblyPath' typeName' methodName' argument' resultPtr
        peek resultPtr

type ExecuteInDefaultAppDomain = ICLRRuntimeHost -> LPCWSTR -> LPCWSTR -> LPCWSTR -> LPCWSTR -> Ptr DWORD -> IO HRESULT
foreign import stdcall "dynamic" makeExecuteInDefaultAppDomain :: FunPtr ExecuteInDefaultAppDomain -> ExecuteInDefaultAppDomain


--
-- Types and functions for programming the COM
--

type HRESULT  = Word32
type CLSID    = Guid
type IID      = Guid

-- | 'InterfacePtr' is a pointer to an arbitrary COM interface (which is a pointer to
--   a vtable of function pointers for the interface methods).
type InterfacePtr = Ptr (Ptr (FunPtr ()))


-- | 'getInterfaceFunction' @i makeFun obj@ is an action that returns the @i@th function
--   of the COM interface referred to by @obj@.  The function is returned as a Haskell
--   function by passing it through @makeFun@.
getInterfaceFunction :: Int -> (FunPtr a -> b) -> InterfacePtr -> IO b
getInterfaceFunction index makeFun this = do
    -- Obtain a pointer to the appropriate element in the vtable for this interface
    funPtr <- peek this >>= (flip peekElemOff) index
    -- Cast the function pointer to the expected type, and import it as a Haskell function
    return $ makeFun $ castFunPtr funPtr


data Guid = Guid Word32 Word16 Word16 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 
    deriving (Show, Eq)

instance Storable Guid where
    sizeOf    _ = 16
    alignment _ = 4

    peek guidPtr = do
        a  <- peek $ plusPtr guidPtr 0 
        b  <- peek $ plusPtr guidPtr 4
        c  <- peek $ plusPtr guidPtr 6
        d0 <- peek $ plusPtr guidPtr 8
        d1 <- peek $ plusPtr guidPtr 9
        d2 <- peek $ plusPtr guidPtr 10
        d3 <- peek $ plusPtr guidPtr 11
        d4 <- peek $ plusPtr guidPtr 12
        d5 <- peek $ plusPtr guidPtr 13
        d6 <- peek $ plusPtr guidPtr 14
        d7 <- peek $ plusPtr guidPtr 15
        return $ Guid a b c d0 d1 d2 d3 d4 d5 d6 d7

    poke guidPtr (Guid a b c d0 d1 d2 d3 d4 d5 d6 d7) = do
        poke (plusPtr guidPtr 0)  a
        poke (plusPtr guidPtr 4)  b
        poke (plusPtr guidPtr 6)  c
        poke (plusPtr guidPtr 8)  d0
        poke (plusPtr guidPtr 9)  d1
        poke (plusPtr guidPtr 10) d2
        poke (plusPtr guidPtr 11) d3
        poke (plusPtr guidPtr 12) d4
        poke (plusPtr guidPtr 13) d5
        poke (plusPtr guidPtr 14) d6
        poke (plusPtr guidPtr 15) d7

-- vim:set ts=4 sw=4 expandtab:
