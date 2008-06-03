{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Salsa.Binding
-- Copyright   : (c) 2007 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE.txt)
-- 
-- Exports data types and functions required by the generated binding files.
--
-----------------------------------------------------------------------------
module Salsa.Binding (
    module Salsa.Common,
    module Salsa.Core,
    module Salsa.CLR,
    module Salsa.TypePrelude,
    module Salsa.Resolver,
    withCWString, CWString, FunPtr, unsafePerformIO, liftM,
    type_GetType_stub
    ) where

import Salsa.Common
import Salsa.Core
import Salsa.CLR
import Salsa.TypePrelude
import Salsa.Resolver

import Foreign hiding (new)
import Foreign.C.String

import Control.Monad (liftM)

-- TODO: Perhaps move some/all of this into the generator, so that it can be
--       CLR-version neutral.

--
-- Import the System.Type.GetType(String) method so it can be used in the
-- implementations of 'typeOf' as produced by the generator.
--

type Type_GetType_stub = CWString -> IO ObjectId
foreign import stdcall "dynamic" make_Type_GetType_stub :: FunPtr Type_GetType_stub -> Type_GetType_stub

{-# NOINLINE type_GetType_stub #-}
type_GetType_stub :: Type_GetType_stub
type_GetType_stub = make_Type_GetType_stub $ unsafePerformIO $ getMethodStub
    "System.Type, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "GetType"
    "System.String"

-- 
-- Typeable instances for primitive types
--

instance Typeable Int32  where typeOf _ = typeOfString "System.Int32"
instance Typeable String where typeOf _ = typeOfString "System.String"
instance Typeable Bool   where typeOf _ = typeOfString "System.Boolean"
instance Typeable Double where typeOf _ = typeOfString "System.Double"

typeOfString :: String -> Obj Type_
typeOfString = unsafePerformIO . marshalMethod1s type_GetType_stub undefined undefined

-- vim:set sw=4 ts=4 expandtab:
