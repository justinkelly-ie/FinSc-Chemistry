module Stage0.Chemistry.ReactionKinetics

import Stage0.BoxInt
import Stage0.Multiset
import Stage1.UnixelFraction
import Stage0.OnSeq.FusedStream
import public Stage0.TransitionNetwork
import Data.Fuel
import Data.List

%default total

------------------------------------------------------------------------
-- 1. ABSTRACT SPECIES TOKENS & TRANSITION NETWORK ALIASES
------------------------------------------------------------------------

||| An abstract chemical species identifier in a discrete reaction network.
public export
data SpeciesToken = SpeciesA | SpeciesB | SpeciesC

public export
Eq SpeciesToken where
  SpeciesA == SpeciesA = True
  SpeciesB == SpeciesB = True
  SpeciesC == SpeciesC = True
  _        == _        = False

public export
Show SpeciesToken where
  show SpeciesA = "A"
  show SpeciesB = "B"
  show SpeciesC = "C"

||| Reaction stoichiometry entry: delta change for a species count.
public export
0 StoichiometryDelta : Type
StoichiometryDelta = Stage0.TransitionNetwork.StoichiometryDelta SpeciesToken

||| Elementary chemical reaction channel R_j:
public export
0 ReactionChannel : Type
ReactionChannel = Stage0.TransitionNetwork.TransitionChannel SpeciesToken

||| Discrete chemical state: multiset tally of species populations.
public export
0 ChemicalState : Type
ChemicalState = TransitionState SpeciesToken

||| Deforested stream generator simulating chemical reaction trajectory over consecutive steps.
public export
streamReactionKinetics : ReactionChannel -> ChemicalState -> FusedStream ChemicalState
streamReactionKinetics = streamTransitionTrajectory

||| Runs a fueled reaction kinetics trajectory using stream fold.
public export
runReactionTrajectory : Fuel -> ReactionChannel -> ChemicalState -> ChemicalState
runReactionTrajectory = runTransitionTrajectory

||| Runs a reaction kinetics trajectory over Nat fuel.
public export
runReactionTrajectoryNat : Nat -> ReactionChannel -> ChemicalState -> ChemicalState
runReactionTrajectoryNat = runTransitionTrajectoryNat

------------------------------------------------------------------------
-- 2. MOLECULAR SPECIES TOKENS & CHEMICAL SYNTHESIS NETWORKS
------------------------------------------------------------------------

||| Molecular species tokens participating in stoichiometric reaction kinetics.
public export
data MolecularSpecies =
    H2        -- Hydrogen gas (H2): 2 H atoms, mass 2
  | O2        -- Oxygen gas (O2): 2 O atoms, mass 32
  | H2O       -- Water (H2O): 2 H + 1 O, mass 18
  | CO2       -- Carbon dioxide: 1 C + 2 O, mass 44
  | CH4       -- Methane: 1 C + 4 H, mass 16
  | N2        -- Nitrogen gas: 2 N atoms, mass 28
  | NH3       -- Ammonia: 1 N + 3 H, mass 17

public export
Eq MolecularSpecies where
  H2  == H2  = True
  O2  == O2  = True
  H2O == H2O = True
  CO2 == CO2 = True
  CH4 == CH4 = True
  N2  == N2  = True
  NH3 == NH3 = True
  _   == _   = False

public export
Show MolecularSpecies where
  show H2  = "H2"
  show O2  = "O2"
  show H2O = "H2O"
  show CO2 = "CO2"
  show CH4 = "CH4"
  show N2  = "N2"
  show NH3 = "NH3"

||| Hydrogen atom count weight table per molecule.
public export
hAtomWeightTable : List (MolecularSpecies, BoxInt)
hAtomWeightTable =
  [ (H2, intToBoxInt 2)
  , (O2, intToBoxInt 0)
  , (H2O, intToBoxInt 2)
  , (CO2, intToBoxInt 0)
  , (CH4, intToBoxInt 4)
  , (N2, intToBoxInt 0)
  , (NH3, intToBoxInt 3)
  ]

||| Oxygen atom count weight table per molecule.
public export
oAtomWeightTable : List (MolecularSpecies, BoxInt)
oAtomWeightTable =
  [ (H2, intToBoxInt 0)
  , (O2, intToBoxInt 2)
  , (H2O, intToBoxInt 1)
  , (CO2, intToBoxInt 2)
  , (CH4, intToBoxInt 0)
  , (N2, intToBoxInt 0)
  , (NH3, intToBoxInt 0)
  ]

||| Carbon atom count weight table per molecule.
public export
cAtomWeightTable : List (MolecularSpecies, BoxInt)
cAtomWeightTable =
  [ (H2, intToBoxInt 0)
  , (O2, intToBoxInt 0)
  , (H2O, intToBoxInt 0)
  , (CO2, intToBoxInt 1)
  , (CH4, intToBoxInt 1)
  , (N2, intToBoxInt 0)
  , (NH3, intToBoxInt 0)
  ]

||| Nitrogen atom count weight table per molecule.
public export
nAtomWeightTable : List (MolecularSpecies, BoxInt)
nAtomWeightTable =
  [ (H2, intToBoxInt 0)
  , (O2, intToBoxInt 0)
  , (H2O, intToBoxInt 0)
  , (CO2, intToBoxInt 0)
  , (CH4, intToBoxInt 0)
  , (N2, intToBoxInt 2)
  , (NH3, intToBoxInt 1)
  ]

------------------------------------------------------------------------
-- 3. CANONICAL CHEMICAL REACTION CHANNELS
------------------------------------------------------------------------

||| Water Synthesis: 2 H2 + O2 -> 2 H2O
public export
waterSynthesisChannel : TransitionChannel MolecularSpecies
waterSynthesisChannel =
  MkChannel 1
    [ MkDelta H2 (intToBoxInt (-2))
    , MkDelta O2 (intToBoxInt (-1))
    , MkDelta H2O (intToBoxInt 2)
    ]
    (intToBoxInt 10)

||| Methane Combustion: CH4 + 2 O2 -> CO2 + 2 H2O
public export
methaneCombustionChannel : TransitionChannel MolecularSpecies
methaneCombustionChannel =
  MkChannel 2
    [ MkDelta CH4 (intToBoxInt (-1))
    , MkDelta O2 (intToBoxInt (-2))
    , MkDelta CO2 (intToBoxInt 1)
    , MkDelta H2O (intToBoxInt 2)
    ]
    (intToBoxInt 25)

||| Haber-Bosch Ammonia Synthesis: N2 + 3 H2 -> 2 NH3
public export
haberBoschChannel : TransitionChannel MolecularSpecies
haberBoschChannel =
  MkChannel 3
    [ MkDelta N2 (intToBoxInt (-1))
    , MkDelta H2 (intToBoxInt (-3))
    , MkDelta NH3 (intToBoxInt 2)
    ]
    (intToBoxInt 5)

------------------------------------------------------------------------
-- 4. CONSERVATION LAW AUDITS
------------------------------------------------------------------------

||| Audits that Water Synthesis preserves Hydrogen and Oxygen atom counts.
public export
auditWaterSynthesisConservation : Bool
auditWaterSynthesisConservation =
  isConservativeDelta hAtomWeightTable waterSynthesisChannel.deltas &&
  isConservativeDelta oAtomWeightTable waterSynthesisChannel.deltas

||| Audits that Methane Combustion preserves Carbon, Hydrogen, and Oxygen atom counts.
public export
auditMethaneCombustionConservation : Bool
auditMethaneCombustionConservation =
  isConservativeDelta cAtomWeightTable methaneCombustionChannel.deltas &&
  isConservativeDelta hAtomWeightTable methaneCombustionChannel.deltas &&
  isConservativeDelta oAtomWeightTable methaneCombustionChannel.deltas

||| Audits that Haber-Bosch Ammonia Synthesis preserves Nitrogen and Hydrogen atom counts.
public export
auditHaberBoschConservation : Bool
auditHaberBoschConservation =
  isConservativeDelta nAtomWeightTable haberBoschChannel.deltas &&
  isConservativeDelta hAtomWeightTable haberBoschChannel.deltas

||| Audits mass-action discrete reaction kinetics over deforested stream transducers:
||| Tests reaction A -> B with stoichiometric conservation (A - 1, B + 1).
public export
auditReactionKineticsProof : Bool
auditReactionKineticsProof =
  let initSt : ChemicalState = AddM SpeciesA (intToBoxInt 10) (AddM SpeciesB (intToBoxInt 0) ZeroM)
      rxn : ReactionChannel = MkChannel 1 [MkDelta SpeciesA (intToBoxInt (-1)), MkDelta SpeciesB (intToBoxInt 1)] (intToBoxInt 1)
      finalSt = runReactionTrajectoryNat 5 rxn initSt
      countA = multiplicity SpeciesA finalSt
      countB = multiplicity SpeciesB finalSt
      totalMass = countA + countB
  in countA == intToBoxInt 5 && countB == intToBoxInt 5 && totalMass == intToBoxInt 10

||| Combined audit verifying all molecular reaction kinetics and atom conservation.
public export
auditMolecularReactionKineticsProof : Bool
auditMolecularReactionKineticsProof =
  auditReactionKineticsProof &&
  auditWaterSynthesisConservation &&
  auditMethaneCombustionConservation &&
  auditHaberBoschConservation
