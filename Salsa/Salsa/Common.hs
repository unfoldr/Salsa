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
    Object_(..),
    Int32,
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

data Object_ = Object_

-- | Represents .NET array types of element type @t@.
data Arr t = Arr t

-- vim:set sw=4 ts=4 expandtab:
