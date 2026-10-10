module Stage0.Chemistry.ReactionKinetics

import Stage0.BoxInt
import Stage0.Multiset
import Stage1.UnixelFraction
import Stage0.OnSeq.FusedStream
import Data.Fuel
import Data.List

%default total

------------------------------------------------------------------------
-- 1. DISCRETE STOCHASTIC CHEMICAL REACTION NETWORK CARRIERS
------------------------------------------------------------------------

||| A chemical species identifier in a discrete reaction network.
public export
data SpeciesToken = SpeciesA | SpeciesB | SpeciesC

public export
Eq SpeciesToken where
  SpeciesA == SpeciesA = True
  SpeciesB == SpeciesB = True
  SpeciesC == SpeciesC = True
  _        == _        = False

||| Reaction stoichiometry entry: delta change for a species count.
public export
record StoichiometryDelta where
  constructor MkDelta
  species : SpeciesToken
  change  : BoxInt

public export
Eq StoichiometryDelta where
  (MkDelta s1 c1) == (MkDelta s2 c2) = s1 == s2 && c1 == c2

||| Elementary chemical reaction channel R_j:
||| Consumes reactants and produces products with an integer propensity rate.
public export
record ReactionChannel where
  constructor MkChannel
  channelId  : Nat
  deltas     : List StoichiometryDelta
  propensity : BoxInt

public export
Eq ReactionChannel where
  (MkChannel id1 d1 p1) == (MkChannel id2 d2 p2) = id1 == id2 && d1 == d2 && p1 == p2

------------------------------------------------------------------------
-- 2. DEFORESTED REACTION NETWORK STATE TRANSDUCER
------------------------------------------------------------------------

||| Discrete chemical state: multiset tally of species populations.
public export
0 ChemicalState : Type
ChemicalState = Multiset BoxInt SpeciesToken

||| Applies a single reaction delta to the chemical state multiset.
public export
applyDelta : ChemicalState -> StoichiometryDelta -> ChemicalState
applyDelta state (MkDelta sp ch) =
  let cur = multiplicity sp state
      newVal = cur + ch
      clamped = if unwrapBox newVal < 0 then intToBoxInt 0 else newVal
  in update sp clamped state
  where
    update : SpeciesToken -> BoxInt -> ChemicalState -> ChemicalState
    update s v ZeroM = AddM s v ZeroM
    update s v (AddM k m rest) =
      if k == s then AddM s v rest
      else AddM k m (update s v rest)

||| Fires a reaction channel, updating all participating species deltas in sequence.
public export
fireChannel : ReactionChannel -> ChemicalState -> ChemicalState
fireChannel (MkChannel _ dList _) state =
  foldl applyDelta state dList

||| Deforested stream generator simulating chemical reaction trajectory over consecutive steps:
||| Generates state trajectory without intermediate list allocations.
public export
streamReactionKinetics : ReactionChannel -> ChemicalState -> FusedStream ChemicalState
streamReactionKinetics channel initState =
  unfoldStream (\st => Yield st (fireChannel channel st)) initState

||| Runs a fueled reaction kinetics trajectory using stream fold.
public export
runReactionTrajectory : Fuel -> ReactionChannel -> ChemicalState -> ChemicalState
runReactionTrajectory f channel initState =
  foldStreamFuel f (\_, nextSt => nextSt) initState (streamReactionKinetics channel initState)

------------------------------------------------------------------------
-- 3. COMPILE-TIME REACTION CONSERVATION AUDIT
------------------------------------------------------------------------

||| Audits mass-action discrete reaction kinetics over deforested stream transducers:
||| Tests reaction A -> B with stoichiometric conservation (A - 1, B + 1).
public export
auditReactionKineticsProof : Bool
auditReactionKineticsProof =
  let initSt : ChemicalState = AddM SpeciesA (intToBoxInt 10) (AddM SpeciesB (intToBoxInt 0) ZeroM)
      rxn : ReactionChannel = MkChannel 1 [MkDelta SpeciesA (intToBoxInt (-1)), MkDelta SpeciesB (intToBoxInt 1)] (intToBoxInt 1)
      finalSt = runReactionTrajectory (limit 5) rxn initSt
      countA = multiplicity SpeciesA finalSt
      countB = multiplicity SpeciesB finalSt
      totalMass = countA + countB
  in unwrapBox countA == 5 && unwrapBox countB == 5 && unwrapBox totalMass == 10
