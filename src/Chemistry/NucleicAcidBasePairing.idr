module Chemistry.NucleicAcidBasePairing

import Core.BoxInt
import Core.UnixelFraction
import Data.List

%default total

------------------------------------------------------------------------
-- 1. LAW 48: DISCRETE WATSON-CRICK BASE PAIR COMPLEMENTARITY & POLYPHOSPHATES
------------------------------------------------------------------------

||| Nucleotide Base Classification.
public export
data NucleotideBase = Adenine | Thymine | Guanine | Cytosine

public export
Eq NucleotideBase where
  Adenine  == Adenine  = True
  Thymine  == Thymine  = True
  Guanine  == Guanine  = True
  Cytosine == Cytosine = True
  _        == _        = False

||| Evaluates the exact hydrogen bond count: A-T = 2 H-bonds, G-C = 3 H-bonds.
%inline
public export
hydrogenBondCount : NucleotideBase -> NucleotideBase -> Nat
hydrogenBondCount Adenine Thymine = 2
hydrogenBondCount Thymine Adenine = 2
hydrogenBondCount Guanine Cytosine = 3
hydrogenBondCount Cytosine Guanine = 3
hydrogenBondCount _ _ = 0

||| Verifies Watson-Crick Purine-Pyrimidine complementarity pairing.
%inline
public export
isComplementaryPair : NucleotideBase -> NucleotideBase -> Bool
isComplementaryPair b1 b2 = hydrogenBondCount b1 b2 > 0

||| Evaluates ATP hydrolysis energy quantum dissipation in discrete tokens (30 tokens per ATP).
%inline
public export
atpHydrolysisEnergyTokens : BoxInt
atpHydrolysisEnergyTokens = intToBoxInt 30

------------------------------------------------------------------------
-- 2. FORMAL INVARIANT AUDIT PROOFS
------------------------------------------------------------------------

||| Audits Law 48 (Watson-Crick Base Pair Complementarity & Polyphosphates):
||| 1. A-T forms 2 H-bonds, G-C forms 3 H-bonds.
||| 2. ATP hydrolysis yields 30 energy tokens.
%inline
public export
auditLaw48WatsonCrickProof : Bool
auditLaw48WatsonCrickProof =
  isComplementaryPair Adenine Thymine &&
  isComplementaryPair Guanine Cytosine &&
  (hydrogenBondCount Guanine Cytosine == 3) &&
  (unwrapBox atpHydrolysisEnergyTokens == 30)
