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
    Obj, null_, Object_(..),
    ( # ), (>>=#),
    new, invoke, set, get, delegate,
    AttrOp(..),
    withCLR, startCLR, stopCLR,
    Int32,
    cast, Coercible
    ) where

import Salsa.Core
import Salsa.Common
import Salsa.CLR

-- vim:set sw=4 ts=4 expandtab:
