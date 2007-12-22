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
    withCWString, CWString, FunPtr, unsafePerformIO
    ) where

import Salsa.Common
import Salsa.Core
import Salsa.CLR
import Salsa.TypePrelude
import Salsa.Resolver

import Foreign hiding (new)
import Foreign.C.String

-- vim:set sw=4 ts=4 expandtab:
