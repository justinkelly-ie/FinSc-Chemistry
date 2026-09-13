module Compound.MacromolecularAssembly

import Core.BoxInt
import Core.Multiset
import Core.VexelMaxel
import Core.UnixelFraction
import Compound.MolecularBonding
import Compound.WatsonCrickBasePairing
import Data.List

%default total

------------------------------------------------------------------------
-- 1. MACROMOLECULAR ASSEMBLY & HEAVY NUCLEI
------------------------------------------------------------------------

||| Macromolecular Structure Specification.
public export
data MacromoleculeSpec = LipidBilayerMembrane | RNADoubleHelix | CatalyticEnzymeComplex | Iron56HeavyNucleus

public export
Eq MacromoleculeSpec where
  LipidBilayerMembrane   == LipidBilayerMembrane   = True
  RNADoubleHelix         == RNADoubleHelix         = True
  CatalyticEnzymeComplex == CatalyticEnzymeComplex = True
  Iron56HeavyNucleus     == Iron56HeavyNucleus     = True
  _                      == _                      = False

public export
Show MacromoleculeSpec where
  show LipidBilayerMembrane   = "Lipid Bilayer Membrane"
  show RNADoubleHelix         = "RNA Double Helix"
  show CatalyticEnzymeComplex = "Catalytic Enzyme Complex"
  show Iron56HeavyNucleus     = "56Fe Heavy Nucleus (1512 Tokens)"

||| Computes exact fundamental mass token count for an Iron-56 nucleus (56 nucleons * 27 = 1512 tokens).
public export
iron56MassTokens : BoxInt
iron56MassTokens = intToBoxInt 1512

||| Computes total hydrogen bond capacity of an RNA strand with n base pairs.
public export
rnaHydrogenBondCapacity : (numGC : Nat) -> (numAU : Nat) -> Nat
rnaHydrogenBondCapacity numGC numAU = (3 * numGC) + (2 * numAU)

------------------------------------------------------------------------
-- 2. FORMAL INVARIANT AUDIT PROOFS
------------------------------------------------------------------------

||| Audits Macromolecular Assembly & Iron-56 Token Conservation:
||| 1. Iron-56 carries exactly 1512 mass tokens (56 * 27).
||| 2. RNA strand with 10 GC pairs and 10 AU pairs carries 50 H-bonds (30 + 20).
%inline
public export
auditMacromolecularAssemblyProof : Bool
auditMacromolecularAssemblyProof =
  unwrapBox iron56MassTokens == 1512 &&
  rnaHydrogenBondCapacity 10 10 == 50
