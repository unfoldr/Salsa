{-# LANGUAGE TypeFamilies, TypeOperators, EmptyDataDecls #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Salsa.TypePrelude
-- Copyright   : (c) 2007 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE.txt)
-- 
-- Type-level implementations of some standard boolean and list functions.
-- Including short-circuited 'and' and 'or' logic functions.
--
-----------------------------------------------------------------------------
module Salsa.TypePrelude where

--
-- Type-level booleans and boolean operations:
--

data TTrue  = TTrue
data TFalse = TFalse

type family TOr x y
type instance TOr TTrue  x  = TTrue
type instance TOr TFalse x  = x

type family TAnd x y
type instance TAnd TFalse x  = TFalse
type instance TAnd TTrue  x  = x

type family TNot x
type instance TNot TTrue  = TFalse
type instance TNot TFalse = TTrue

type family If c a b
type instance If TTrue  a b = a
type instance If TFalse a b = b

--
-- Type-level lists and associated operations:
--

data TNil
data x ::: xs -- = x ::: xs
infixr 5 :::

type family    BoolEq x       y
type instance  BoolEq TTrue   TTrue   = TTrue
type instance  BoolEq TFalse  TFalse  = TTrue
type instance  BoolEq TFalse  TTrue   = TFalse
type instance  BoolEq TTrue   TFalse  = TFalse

-- | 'ListEq xs ys' is true if the given type-level boolean lists contain 
--   the same boolean values, and false otherwise.
type family    ListEq xs          ys
type instance  ListEq TNil        TNil        = TTrue
type instance  ListEq (x ::: xs)  TNil        = TFalse
type instance  ListEq TNil        (x ::: xs)  = TFalse
type instance  ListEq (x ::: xs)  (y ::: ys)  = TAnd (BoolEq x y) (ListEq xs ys)

-- | @'FromSingleton' xs def@ returns the only element of the list @xs@ (if it is a
--   single element list), otherwise it returns the default value @def@.
type family FromSingleton def xs
type instance FromSingleton def TNil            = def
type instance FromSingleton def (x ::: TNil)    = x
type instance FromSingleton def (x ::: y ::: z) = def

-- vim:set sw=4 ts=4 expandtab:
