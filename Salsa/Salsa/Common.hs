{-# LANGUAGE EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Salsa.Common
-- Copyright   : (c) 2007 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE.txt)
-- 
-- Exports data types and functions that are used throughout the bridge
-- implementation.
--
-----------------------------------------------------------------------------
module Salsa.Common (
    Obj(..),
    Null,
    Object_(..),      _Object,
    Type_(..),        _Type,
    Int32,   Int32_,  _Int32,
    String,  String_, _String,
    Arr(..)
    ) where

import Foreign hiding (new, newForeignPtr)

import Salsa.TypePrelude
import Salsa.CLR

-- | @Obj a@ represents a .NET object instance of type @a@.
data Obj a = Obj !ObjectId !(ForeignPtr ()) 
           | ObjNull

data Null

instance Show (Obj a) where
    show (Obj id _) = "Object(" ++ show id ++ ")"

-- Labels for elementary .NET types
data Object_ = Object_                  -- System.Object
data Type_   = Type_                    -- System.Type
_Type   = undefined :: Type_
_Object = undefined :: Object_

-- Synonym labels for the primitive types
type String_ = String
type Int32_  = Int32
_String = undefined :: String_
_Int32  = undefined :: Int32_

-- | Represents .NET array types of element type @t@.
data Arr t = Arr t

-- vim:set sw=4 ts=4 expandtab:
