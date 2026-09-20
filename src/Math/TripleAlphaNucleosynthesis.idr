module Math.TripleAlphaNucleosynthesis

import Core.BoxInt
import Math.Multiset
import Data.List



%default total

------------------------------------------------------------------------
-- 1. LAW 52: DISCRETE TRIPLE-ALPHA CARBON & PHOSPHORUS NUCLEOSYNTHESIS
------------------------------------------------------------------------

||| Evaluates Triple-Alpha fusion mass token conservation: 3 * Alpha (108) = Carbon-12 (324).
%inline
public export
tripleAlphaFusionTokens : BoxInt -> BoxInt
tripleAlphaFusionTokens alphaMass = intToBoxInt 3 * alphaMass

||| Evaluates Phosphorus-31 synthesis token count (837 tokens = 31 * 27).
%inline
public export
phosphorus31MassTokens : BoxInt
phosphorus31MassTokens = intToBoxInt 837

------------------------------------------------------------------------
-- 1B. PURE MULTISET ISOTOPE REACTION STOICHIOMETRY
------------------------------------------------------------------------

||| Fundamental Isotope Token Carrier for Stellar Nucleosynthesis
public export
data IsotopeToken = AlphaParticle | Carbon12 | Phosphorus31

public export
Eq IsotopeToken where
  AlphaParticle == AlphaParticle = True
  Carbon12      == Carbon12      = True
  Phosphorus31  == Phosphorus31  = True
  _             == _             = False

||| Evaluates total mass token count for an Isotope token.
public export
isotopeMass : IsotopeToken -> BoxInt
isotopeMass AlphaParticle = intToBoxInt 108   -- 4 amu = 108 primorial mass tokens
isotopeMass Carbon12      = intToBoxInt 324   -- 12 amu = 324 primorial mass tokens
isotopeMass Phosphorus31  = intToBoxInt 837   -- 31 amu = 837 primorial mass tokens

||| Computes total mass of an Isotope multiset.
public export
multisetIsotopeMass : Multiset BoxInt IsotopeToken -> BoxInt
multisetIsotopeMass ZeroM = intToBoxInt 0
multisetIsotopeMass (AddM iso count rest) = (isotopeMass iso * count) + multisetIsotopeMass rest

||| Triple-Alpha Fusion reaction multiset: 3 * AlphaParticle.
public export
tripleAlphaReactants : Multiset BoxInt IsotopeToken
tripleAlphaReactants = AddM AlphaParticle (intToBoxInt 3) ZeroM

||| Triple-Alpha Fusion product multiset: 1 * Carbon12.
public export
tripleAlphaProducts : Multiset BoxInt IsotopeToken
tripleAlphaProducts = AddM Carbon12 (intToBoxInt 1) ZeroM

||| Proves that Triple-Alpha Fusion reaction conserves mass token count:
||| Mass(3 * AlphaParticle) == Mass(1 * Carbon12) = 324.
public export
auditMultisetTripleAlphaMassConservation : Bool
auditMultisetTripleAlphaMassConservation =
  let reactantMass = multisetIsotopeMass tripleAlphaReactants
      productMass  = multisetIsotopeMass tripleAlphaProducts
  in reactantMass == productMass && unwrapBox reactantMass == 324

------------------------------------------------------------------------
-- 2. FORMAL INVARIANT AUDIT PROOFS
------------------------------------------------------------------------

||| Audits Law 52 (Triple-Alpha Carbon & Phosphorus Nucleosynthesis):
||| 1. 3 * 108 = 324 mass tokens for 12C core.
||| 2. 31P carries 837 mass tokens (31 amu).
||| 3. Pure multiset reaction mass conservation (Mass(3 alpha) == Mass(1 12C)).
%inline
public export
auditLaw52TripleAlphaProof : Bool
auditLaw52TripleAlphaProof =
  let c12 = tripleAlphaFusionTokens (intToBoxInt 108)
  in (unwrapBox c12 == 324) &&
     (unwrapBox phosphorus31MassTokens == 837) &&
     auditMultisetTripleAlphaMassConservation

