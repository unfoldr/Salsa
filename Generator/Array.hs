--
-- Define instance members (Candidates and Invoker), 
--        constructors (Candidates and Invoker),
--        properties, and
--        supertypes (SupertypesOf)
-- for all array types, in terms of the System.Array members they inherit.
--
-- Note that .NET arrays have no static members.
--

-- Instance methods for arrays are the same as the System.Array instance methods:
type instance Candidates (Obj (Arr t)) member = Candidates (Obj Array_) member

-- Delegate all array instance method invocations to System.Array:
instance Invoker (Obj Array_) m args => Invoker (Obj (Arr t)) m args where
  type Result (Obj (Arr t)) m args = Result (Obj Array_) m args
  rawInvoke t m args = rawInvoke (convert t :: Obj Array_) m args


-- There is one constructor for single-dimensional arrays which just calls the
-- static Array.CreateInstance method passing the appropriate type and length:
type instance Candidates (Arr t) Ctor = (Int32 ::: TNil) ::: TNil
instance Typeable t => Invoker (Arr t) Ctor (Int32) where
  type Result (Arr t) Ctor (Int32) = IO (Obj (Arr t))
  rawInvoke t m (len) = liftM cast $ _Array # _createInstance (typeOf (undefined :: t), len)


-- Delegate all array instance property gets/sets to System.Array:
instance Prop (Obj Array_) pn => Prop (Obj (Arr t)) pn where
  type PropGT (Obj (Arr t)) pn = PropGT (Obj Array_) pn
  getProp t = getProp (convert t :: Obj Array_)
  type PropST (Obj (Arr t)) pn = PropST (Obj Array_) pn
  setProp t = setProp (convert t :: Obj Array_)

-- All arrays derive directly from System.Array:
type instance SupertypesOf (Obj (Arr t)) = (Obj Array_) ::: SupertypesOf (Obj Array_)


-- Define the type code of single-dimension arrays in terms of the underlying
-- element type:
type instance TyCode (Obj (Arr t)) = D0 ::: DF ::: TyCode t
-- (Note the use of 'DF' to separate the type code of the element from the type code 
-- of arrays in general.)

-- EXPERIMENTAL:
type instance FromTyCode (D0 ::: DF ::: t) = Obj (Arr (FromTyCode t))

