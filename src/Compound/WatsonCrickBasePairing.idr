module Compound.WatsonCrickBasePairing

import Core
import Transform
import Data.Vect

%default total

------------------------------------------------------------------------
-- 1. WATSON-CRICK COMPLEMENTARY NUCLEOBASES (EPOCH 37)
------------------------------------------------------------------------

||| The 5 Fundamental Genetic Nucleobases.
public export
data Nucleobase = Adenine | Thymine | Guanine | Cytosine | Uracil

public export
Eq Nucleobase where
  Adenine  == Adenine  = True
  Thymine  == Thymine  = True
  Guanine  == Guanine  = True
  Cytosine == Cytosine = True
  Uracil   == Uracil   = True
  _        == _        = False

public export
Show Nucleobase where
  show Adenine  = "Adenine (A)"
  show Thymine  = "Thymine (T)"
  show Guanine  = "Guanine (G)"
  show Cytosine = "Cytosine (C)"
  show Uracil   = "Uracil (U)"

||| Determines the Watson-Crick complementary base partner.
public export
complementaryBase : Nucleobase -> Nucleobase
complementaryBase Adenine  = Thymine
complementaryBase Thymine  = Adenine
complementaryBase Guanine  = Cytosine
complementaryBase Cytosine = Guanine
complementaryBase Uracil   = Adenine

||| Evaluates the exact discrete Hydrogen Bond count between two nucleobases:
||| - Adenine = Thymine: exactly 2 Hydrogen Bonds
||| - Adenine = Uracil:  exactly 2 Hydrogen Bonds
||| - Guanine ≡ Cytosine: exactly 3 Hydrogen Bonds (matching the triadic 3-color ratio)
||| - Mismatched pairs: 0 Hydrogen Bonds
public export
basePairHBondCount : Nucleobase -> Nucleobase -> Nat
basePairHBondCount Adenine  Thymine  = 2
basePairHBondCount Thymine  Adenine  = 2
basePairHBondCount Adenine  Uracil   = 2
basePairHBondCount Uracil   Adenine  = 2
basePairHBondCount Guanine  Cytosine = 3
basePairHBondCount Cytosine Guanine  = 3
basePairHBondCount _        _        = 0

------------------------------------------------------------------------
-- 2. HIGH-ENERGY POLYPHOSPHATE (ATP) THERMODYNAMIC COUPLING
--    Provides the necessary free energy (ΔG < 0) to drive endergonic polymerization
------------------------------------------------------------------------

||| An Adenosine Triphosphate (ATP) energy carrier molecule.
public export
record ATPMolecule where
  constructor MkATPMolecule
  phosphateGroups : Nat -- 3 (Alpha, Beta, Gamma)
  phosphoAnhydrideBonds : Nat -- 2 high-energy P-O-P bonds

||| Canonical ATP ground state.
public export
seedATP : ATPMolecule
seedATP = MkATPMolecule 3 2

||| Product of ATP Hydrolysis: ATP + H2O -> ADP + Pi + Discrete Energy Tokens.
public export
record HydrolysisResult where
  constructor MkHydrolysisResult
  adpPhosphates : Nat -- 2
  inorganicPhosphate : Nat -- 1
  energyTokensReleased : Nat -- Discrete thermodynamic drive (30 kJ/mol equivalent)

||| Executes ATP Hydrolysis: Cleaves 1 phosphoanhydride bond, releasing 1 Pi and energy.
public export
hydrolyzeATP : ATPMolecule -> HydrolysisResult
hydrolyzeATP (MkATPMolecule 3 2) = MkHydrolysisResult 2 1 1
hydrolyzeATP (MkATPMolecule p b) = MkHydrolysisResult (p `minus` 1) 1 1

------------------------------------------------------------------------
-- 3. CONSTRUCTIVE FORMAL AUDIT PROOFS
------------------------------------------------------------------------

||| Audits the Watson-Crick Hydrogen Bond Invariant:
||| 1. A-T base pair forms exactly 2 Hydrogen bonds.
||| 2. G-C base pair forms exactly 3 Hydrogen bonds.
||| 3. Mismatched pair (A-C) forms 0 Hydrogen bonds.
public export
auditWatsonCrickHydrogenBondRatioProof : Bool
auditWatsonCrickHydrogenBondRatioProof =
  let atBonds = basePairHBondCount Adenine Thymine
      gcBonds = basePairHBondCount Guanine Cytosine
      acBonds = basePairHBondCount Adenine Cytosine
  in atBonds == 2 &&
     gcBonds == 3 &&
     acBonds == 0

||| Audits the Pyrophosphate (ATP) Thermodynamic Coupling Invariant:
||| Hydrolysis of seed ATP strictly produces ADP (2 phosphates) + 1 Pi + 1 energy token.
public export
auditPyrophosphateThermodynamicCouplingProof : Bool
auditPyrophosphateThermodynamicCouplingProof =
  let atp = seedATP
      result = hydrolyzeATP atp
  in phosphoAnhydrideBonds atp == 2 &&
     adpPhosphates result == 2 &&
     inorganicPhosphate result == 1 &&
     energyTokensReleased result == 1

------------------------------------------------------------------------
-- 4. UNIVERSAL TRANSFORM MULTISET BASE PAIRING INSTANCE
------------------------------------------------------------------------

||| Canonical Base Pairing Target Class.
public export
data BasePairClass = AT_Pair | GC_Pair

public export
Eq BasePairClass where
  AT_Pair == AT_Pair = True
  GC_Pair == GC_Pair = True
  _ == _ = False

||| Base Pairing Transform Multiset (G ⊗ Z ⊗ J):
||| G: EllipticSector (Bound State Confinement)
||| Z: 1 / [1] (Exact 1:1 pairing ratio)
||| J: Nucleobase -> BasePairClass
public export
basePairingTransform : MaxelTransform Nucleobase BasePairClass
basePairingTransform = mkTransformBox EllipticSector unitUnixelFraction
  [ ((Adenine, AT_Pair), intToBoxInt 1)
  , ((Thymine, AT_Pair), intToBoxInt 1)
  , ((Guanine, GC_Pair), intToBoxInt 1)
  , ((Cytosine, GC_Pair), intToBoxInt 1)
  , ((Uracil, AT_Pair), intToBoxInt 1)
  ]

||| Audits Base Pairing Transform Multiset application.
public export
auditBasePairingTransformProof : Bool
auditBasePairingTransformProof =
  let dnaBox : Box Nucleobase = insertBox Adenine (intToBoxInt 1) (insertBox Thymine (intToBoxInt 1) emptyBox)
      pushed = applyPushforwardContraction basePairingTransform dnaBox
  in lookupBox AT_Pair pushed == intToBoxInt 2
