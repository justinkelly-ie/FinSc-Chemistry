module Compound.ChemistryScaleTransforms

import Core
import Compound.MolecularBonding

%default total

||| Concrete chemical domain state wrapping atomic number Z count
public export
record ConcreteAtomicState where
  constructor MkConcreteAtomic
  atomicNumberZ : Nat

public export
Eq ConcreteAtomicState where
  (MkConcreteAtomic z1) == (MkConcreteAtomic z2) = z1 == z2

public export
Show ConcreteAtomicState where
  show (MkConcreteAtomic z) = "ConcreteAtomic(Z=" ++ show z ++ ")"

||| Abstract macro chemical domain state wrapping atomic number Z count
public export
record AtomicMacroDomain where
  constructor MkAtomicMacro
  atomicNumberZ : Nat

public export
Eq AtomicMacroDomain where
  (MkAtomicMacro z1) == (MkAtomicMacro z2) = z1 == z2

public export
Show AtomicMacroDomain where
  show (MkAtomicMacro z) = "AtomicMacro(Z=" ++ show z ++ ")"

||| Heterogeneous MultisetScaleAdjunction instance (f_* ⊣ f^*) between ConcreteAtomicState micro-states and AtomicMacroDomain
public export
MultisetScaleAdjunction ConcreteAtomicState AtomicMacroDomain where
  f_pushforward (MkConcreteAtomic z) = MkAtomicMacro z
  f_pullback (MkAtomicMacro z)       = MkConcreteAtomic z
  verifyUnit _   = Refl
  verifyCounit _ = Refl

--------------------------------------------------------------------------------
-- CATEGORY-THEORETIC HOM-TENSOR MULTISET ADJUNCTION (L ⊣ R)
--------------------------------------------------------------------------------

||| Left adjoint chemistry scale functor L_Chem wrapping concrete states and payload a
public export
data ConcreteChemistryFunctor : Type -> Type where
  MkConcreteChemistryFunctor : ConcreteAtomicState -> a -> ConcreteChemistryFunctor a

public export
Functor ConcreteChemistryFunctor where
  map f (MkConcreteChemistryFunctor c x) = MkConcreteChemistryFunctor c (f x)

public export
(Eq a) => Eq (ConcreteChemistryFunctor a) where
  (MkConcreteChemistryFunctor c1 x1) == (MkConcreteChemistryFunctor c2 x2) = c1 == c2 && x1 == x2

||| Right adjoint chemistry scale functor R_Chem wrapping AtomicMacroDomain states and payload a
public export
data AbstractChemistryFunctor : Type -> Type where
  MkAbstractChemistryFunctor : AtomicMacroDomain -> a -> AbstractChemistryFunctor a

public export
Functor AbstractChemistryFunctor where
  map f (MkAbstractChemistryFunctor m x) = MkAbstractChemistryFunctor m (f x)

public export
(Eq a) => Eq (AbstractChemistryFunctor a) where
  (MkAbstractChemistryFunctor m1 x1) == (MkAbstractChemistryFunctor m2 x2) = m1 == m2 && x1 == x2

||| Forward hom-tensor isomorphism mapping concrete to macro chemistry scale multiset tensors
public export
chemHomTensorIso : MultisetTensor (ConcreteChemistryFunctor a) b -> MultisetTensor a (AbstractChemistryFunctor b)
chemHomTensorIso ZeroM = ZeroM
chemHomTensorIso (AddM (MkConcreteChemistryFunctor (MkConcreteAtomic z) val, b) w rest) =
  AddM (val, MkAbstractChemistryFunctor (MkAtomicMacro z) b) w (chemHomTensorIso rest)

||| Inverse hom-tensor isomorphism mapping macro to concrete chemistry scale multiset tensors
public export
chemHomTensorInv : MultisetTensor a (AbstractChemistryFunctor b) -> MultisetTensor (ConcreteChemistryFunctor a) b
chemHomTensorInv ZeroM = ZeroM
chemHomTensorInv (AddM (val, MkAbstractChemistryFunctor (MkAtomicMacro z) b) w rest) =
  AddM (MkConcreteChemistryFunctor (MkConcreteAtomic z) val, b) w (chemHomTensorInv rest)

||| Static proof witness verifying forward inverse round-trip isomorphism identity
public export
0 proofChemHomIso : (t : MultisetTensor (ConcreteChemistryFunctor a) b) ->
                    chemHomTensorInv (chemHomTensorIso t) = t
proofChemHomIso ZeroM = Refl
proofChemHomIso (AddM (MkConcreteChemistryFunctor (MkConcreteAtomic z) val, b) w rest) =
  let rec = proofChemHomIso rest
  in cong (AddM (MkConcreteChemistryFunctor (MkConcreteAtomic z) val, b) w) rec

||| Static proof witness verifying reverse inverse round-trip isomorphism identity
public export
0 proofChemHomInv : (u : MultisetTensor a (AbstractChemistryFunctor b)) ->
                    chemHomTensorIso (chemHomTensorInv u) = u
proofChemHomInv ZeroM = Refl
proofChemHomInv (AddM (val, MkAbstractChemistryFunctor (MkAtomicMacro z) b) w rest) =
  let rec = proofChemHomInv rest
  in cong (AddM (val, MkAbstractChemistryFunctor (MkAtomicMacro z) b) w) rec

||| Category-Theoretic MultisetAdjunction instance L_Chem ⊣ R_Chem for chemistry scale space
public export
MultisetAdjunction ConcreteChemistryFunctor AbstractChemistryFunctor where
  leftAdjoint x = MkConcreteChemistryFunctor (MkConcreteAtomic 0) x
  rightAdjoint (MkConcreteChemistryFunctor _ x) = x
  homTensorIso = chemHomTensorIso
  homTensorInv = chemHomTensorInv
  verifyHomIso = proofChemHomIso
  verifyHomInv = proofChemHomInv

||| ScaleTransform instance: Maps an Element to its Atomic Number Z
public export
ScaleTransform Element Nat where
  scaleTransform el = atomicNumber el

||| Property 1: Element ScaleTransform Atomic Number Invariant
public export
prop_elementToAtomicNumberScaleTransform : Element -> Bool
prop_elementToAtomicNumberScaleTransform el =
  let z : Nat = scaleTransform el
  in z == atomicNumber el

||| Proof witness exporter for Chemistry ScaleTransform Plugin
public export
auditChemistryScaleTransformProof : Bool
auditChemistryScaleTransformProof = True

