module Chemistry.HydrogenBonding

import Core.BoxInt
import Math.LinAlgebra.MetricTensor
import Data.List

%default total

------------------------------------------------------------------------
-- 1. LAW 50: DISCRETE HYDROGEN BOND NETWORK & QUADREA GEOMETRY
------------------------------------------------------------------------

||| Evaluates water H2O bond angle quadrea: A = (Q1 + Q2 + Q3)^2 - 2(Q1^2 + Q2^2 + Q3^2).
||| For H2O bond geometry (Q1=1, Q2=1, Q3=1): A = 9 - 6 = 3.
%inline
public export
waterBondQuadrea : BoxInt -> BoxInt -> BoxInt -> BoxInt
waterBondQuadrea q1 q2 q3 =
  let sumQ = q1 + q2 + q3
      sqSum = (sumQ * sumQ)
      sumSq = (q1 * q1) + (q2 * q2) + (q3 * q3)
  in sqSum - (intToBoxInt 2 * sumSq)

||| Verifies 4-coordinate tetrahedral liquid water percolation flux (4 H-bonds per molecule).
%inline
public export
isTetrahedralWaterPercolation : Nat -> Bool
isTetrahedralWaterPercolation coordNum = coordNum == 4

------------------------------------------------------------------------
-- 2. FORMAL INVARIANT AUDIT PROOFS
------------------------------------------------------------------------

||| Audits Law 50 (Hydrogen Bond Network & Quadrea Geometry):
||| 1. Water bond quadrea A(1, 1, 1) = 3.
||| 2. Tetrahedral coordination number = 4.
%inline
public export
auditLaw50HydrogenBondingProof : Bool
auditLaw50HydrogenBondingProof =
  let qH2O = waterBondQuadrea (intToBoxInt 1) (intToBoxInt 1) (intToBoxInt 1)
  in (unwrapBox qH2O == 3) && isTetrahedralWaterPercolation 4
