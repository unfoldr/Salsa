-----------------------------------------------------------------------------
-- |
-- Module      : Salsa
-- Copyright   : (c) 2007 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE.txt)
-- 
-- Salsa: a .NET bridge for Haskell.
--
-----------------------------------------------------------------------------
module Salsa (
    Obj, null_,
    ( # ), (>>=#),
    new, invoke, set, get, delegate,
    AttrOp(..),
    withCLR, startCLR, stopCLR,
    Arr(..),
    Int32,
    cast, Coercible,
    Object_(..), _Object,
    Type_(..),   _Type,
    Int32_,      _Int32,
    String_,     _String,
    ) where

import Salsa.Core
import Salsa.Common
import Salsa.CLR

-- vim:set sw=4 ts=4 expandtab:
