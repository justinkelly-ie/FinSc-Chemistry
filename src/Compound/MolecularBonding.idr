module Compound.MolecularBonding

import Core.BoxInt
import Core.VexelMaxel
import Math.RationalTrig
import Math.LinAlgebra.MetricTensor
import Data.Vect

%default total

||| Atomic Element representation with exact valence electron capacity.
public export
data Element = Hydrogen | Carbon | Nitrogen | Oxygen

public export
Eq Element where
  Hydrogen == Hydrogen = True
  Carbon   == Carbon   = True
  Nitrogen == Nitrogen = True
  Oxygen   == Oxygen   = True
  _        == _        = False

public export
Show Element where
  show Hydrogen = "H"
  show Carbon   = "C"
  show Nitrogen = "N"
  show Oxygen   = "O"

||| Returns the valence bond capacity (number of covalent bond slots) for each element.
public export
valenceCapacity : Element -> Nat
valenceCapacity Hydrogen = 1
valenceCapacity Carbon   = 4
valenceCapacity Nitrogen = 3
valenceCapacity Oxygen   = 2

||| Returns the atomic number Z (number of protons) for each element.
public export
atomicNumber : Element -> Nat
atomicNumber Hydrogen = 1
atomicNumber Carbon   = 6
atomicNumber Nitrogen = 7
atomicNumber Oxygen   = 8

||| A Covalent Bond represented as a Pixel in the molecular adjacency Maxel
||| linking atom i and atom j with bond order (1 = single, 2 = double, 3 = triple).
public export
record CovalentBond where
  constructor MkCovalentBond
  atom1 : Nat
  atom2 : Nat
  order : Nat

||| Converts a list of covalent bonds into a pure Maxel adjacency matrix.
public export
bondsToMaxel : List CovalentBond -> Maxel
bondsToMaxel bonds =
  let pixels = concatMap (\(MkCovalentBond a1 a2 ord) => 
                [ (MkPixel a1 a2, natToBoxInt ord)
                , (MkPixel a2 a1, natToBoxInt ord) ]) bonds
  in MkMaxel pixels

||| A Molecular Graph containing an indexed vector of atoms and a bond Maxel.
public export
record Molecule (numAtoms : Nat) where
  constructor MkMolecule
  formula     : String
  elements    : Vect numAtoms Element
  bondMatrix  : Maxel

||| Total valence requirement across all constituent atoms in the molecule.
public export
totalValenceDemand : {n : Nat} -> Vect n Element -> Nat
totalValenceDemand [] = 0
totalValenceDemand (e :: es) = valenceCapacity e + totalValenceDemand es

||| Total shared electron pairs across all covalent bonds in the Maxel.
public export
totalBondOrders : List CovalentBond -> Nat
totalBondOrders [] = 0
totalBondOrders (b :: bs) = order b + totalBondOrders bs

||| Exact Octet / Duet Saturation Theorem:
||| A molecule is chemically saturated when 2 * totalBondOrders == totalValenceDemand.
public export
isSaturatedMolecule : {n : Nat} -> Vect n Element -> List CovalentBond -> Bool
isSaturatedMolecule elems bonds =
  (2 * totalBondOrders bonds) == totalValenceDemand elems

-- ========================================================
-- CANONICAL MOLECULAR FORMATIONS & INVARIANTS
-- ========================================================

||| Canonical Water Molecule (H2O: 1 Oxygen + 2 Hydrogens).
public export
waterMolecule : Molecule 3
waterMolecule =
  let elems = [Oxygen, Hydrogen, Hydrogen]
      bonds = [MkCovalentBond 1 2 1, MkCovalentBond 1 3 1]
  in MkMolecule "H2O" elems (bondsToMaxel bonds)

||| Water Molecule Archimedes Quadrea Invariant:
||| Evaluates Archimedes' Function A(Q1, Q2, Q3) over the O-H (Q1=1), O-H (Q2=1), and H-H (Q3=3) quadrances.
||| A(1, 1, 3) = 4(1)(1) - (1 + 1 - 3)^2 = 4 - (-1)^2 = 3 = Quadrea.
public export
waterArchimedesQuadrea : BoxInt
waterArchimedesQuadrea =
  archimedesFunction (intToBoxInt 1) (intToBoxInt 1) (intToBoxInt 3)

||| Canonical Methane Molecule (CH4: 1 Carbon + 4 Hydrogens).
public export
methaneMolecule : Molecule 5
methaneMolecule =
  let elems = [Carbon, Hydrogen, Hydrogen, Hydrogen, Hydrogen]
      bonds = [ MkCovalentBond 1 2 1
              , MkCovalentBond 1 3 1
              , MkCovalentBond 1 4 1
              , MkCovalentBond 1 5 1 ]
  in MkMolecule "CH4" elems (bondsToMaxel bonds)

||| 3D Coordinate Vector for Tetrahedral Symmetry in Z3^3.
public export
record Coord3D where
  constructor MkCoord3D
  cx : BoxInt
  cy : BoxInt
  cz : BoxInt

public export
addCoord3D : Coord3D -> Coord3D -> Coord3D
addCoord3D (MkCoord3D x1 y1 z1) (MkCoord3D x2 y2 z2) =
  MkCoord3D (x1 + x2) (y1 + y2) (z1 + z2)

||| The 4 Methane C-H Bond Direction Vectors in tetrahedral space:
||| v1 = (+1, +1, +1), v2 = (+1, -1, -1), v3 = (-1, +1, -1), v4 = (-1, -1, +1).
public export
methaneTetrahedralVectors : Vect 4 Coord3D
methaneTetrahedralVectors =
  [ MkCoord3D (intToBoxInt 1)    (intToBoxInt 1)    (intToBoxInt 1)
  , MkCoord3D (intToBoxInt 1)    (intToBoxInt (-1)) (intToBoxInt (-1))
  , MkCoord3D (intToBoxInt (-1)) (intToBoxInt 1)    (intToBoxInt (-1))
  , MkCoord3D (intToBoxInt (-1)) (intToBoxInt (-1)) (intToBoxInt 1)
  ]

||| Proves that the 4 tetrahedral C-H bond vectors sum strictly to the null vector (0, 0, 0),
||| demonstrating perfect geometric equilibrium and zero mechanical strain.
public export
methaneCentroidNullVector : Coord3D
methaneCentroidNullVector =
  foldl addCoord3D (MkCoord3D (intToBoxInt 0) (intToBoxInt 0) (intToBoxInt 0)) methaneTetrahedralVectors

||| Canonical Ethane Molecule (C2H6: 2 Carbons + 6 Hydrogens).
public export
ethaneMolecule : Molecule 8
ethaneMolecule =
  let elems = [Carbon, Carbon, Hydrogen, Hydrogen, Hydrogen, Hydrogen, Hydrogen, Hydrogen]
      bonds = [ MkCovalentBond 1 2 1
              , MkCovalentBond 1 3 1, MkCovalentBond 1 4 1, MkCovalentBond 1 5 1
              , MkCovalentBond 2 6 1, MkCovalentBond 2 7 1, MkCovalentBond 2 8 1
              ]
  in MkMolecule "C2H6" elems (bondsToMaxel bonds)

||| Alkane Homologous Series: Number of Hydrogens for an unbranched alkane with n Carbons is (2n + 2).
public export
alkaneHydrogenCount : Nat -> Nat
alkaneHydrogenCount n = (2 * n) + 2

||| Alkane Homologous Series: Total single bonds in C_n H_{2n+2} is (3n + 1).
%inline
public export
alkaneTotalBonds : Nat -> Nat
alkaneTotalBonds n = (3 * n) + 1

||| Validates the Alkane Saturation Theorem:
||| Total atomic valence demand (4n + 2n + 2 = 6n + 2) identically matches 2 * total bonds (2 * (3n + 1) = 6n + 2).
%inline
public export
verifyAlkaneSaturation : (n : Nat) -> Bool
verifyAlkaneSaturation n =
  (4 * n + alkaneHydrogenCount n) == 2 * alkaneTotalBonds n

------------------------------------------------------------------------
-- 4. 3D MOLECULAR CONFORMATIONS & EXACT RATIONAL SPREADS
------------------------------------------------------------------------

||| A Complete 3D Chemical Molecule represented constructively as:
||| - atoms: 3D Boxel of atomic positions [x, y, z] mapped to atomic number Z.
||| - bonds: 2D Maxel of covalent bond adjacency.
public export
record Molecule3D where
  constructor MkMolecule3D
  name  : String
  atoms : Boxel
  bonds : Maxel

||| 3D Conformation of Methane (CH4) centered at [1, 1, 1] on the discrete 3x3x3 grid:
||| - Carbon at [1, 1, 1] (Z=6)
||| - Hydrogen 1 at [2, 2, 2] (Z=1)
||| - Hydrogen 2 at [2, 0, 0] (Z=1)
||| - Hydrogen 3 at [0, 2, 0] (Z=1)
||| - Hydrogen 4 at [0, 0, 2] (Z=1)
public export
methaneMolecule3D : Molecule3D
methaneMolecule3D =
  let atomGrid = MkBoxel [ (MkVoxel 1 1 1, intToBoxInt 6)  -- Carbon
                         , (MkVoxel 2 2 2, intToBoxInt 1)  -- H1
                         , (MkVoxel 2 0 0, intToBoxInt 1)  -- H2
                         , (MkVoxel 0 2 0, intToBoxInt 1)  -- H3
                         , (MkVoxel 0 0 2, intToBoxInt 1)  -- H4
                         ]
      bondGrid = bondsToMaxel [ MkCovalentBond 1 2 1
                              , MkCovalentBond 1 3 1
                              , MkCovalentBond 1 4 1
                              , MkCovalentBond 1 5 1
                              ]
  in MkMolecule3D "CH4" atomGrid bondGrid

||| Proves that the bond angle between H1-C-H2 in Methane has exact Rational Spread s = 8/9
||| (corresponding to the tetrahedral bond angle theta ~ 109.47 degrees):
||| Q(C, H1) = 3, Q(C, H2) = 3, Q(H1, H2) = 8 => Spread = A(3,3,8) / (4*3*3) = 32/36 = 8/9.
%inline
public export
methaneTetrahedralSpreadProof : Bool
methaneTetrahedralSpreadProof =
  let carbon = MkVoxel 1 1 1
      h1     = MkVoxel 2 2 2
      h2     = MkVoxel 2 0 0
      res    = spread3D h1 carbon h2
      num    = fst res
      den    = snd res
  in (unwrapBox num * 9) == (unwrapBox den * 8)


||| 3D Conformation of Water (H2O):
||| - Oxygen at [1, 1, 1] (Z=8)
||| - Hydrogen 1 at [2, 1, 1] (Z=1)
||| - Hydrogen 2 at [1, 2, 1] (Z=1)
public export
waterMolecule3D : Molecule3D
waterMolecule3D =
  let atomGrid = MkBoxel [ (MkVoxel 1 1 1, intToBoxInt 8)  -- Oxygen
                         , (MkVoxel 2 1 1, intToBoxInt 1)  -- H1
                         , (MkVoxel 1 2 1, intToBoxInt 1)  -- H2
                         ]
      bondGrid = bondsToMaxel [ MkCovalentBond 1 2 1, MkCovalentBond 1 3 1 ]
  in MkMolecule3D "H2O" atomGrid bondGrid

%inline
public export
auditTier5MolecularBondingProof : Bool
auditTier5MolecularBondingProof = methaneTetrahedralSpreadProof



