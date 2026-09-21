# FinSc-Chemistry

[![Idris 2 Verification](https://img.shields.io/badge/Idris_2-0.8.0-blue.svg)](https://www.idris-lang.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**Layer 5b Substrate-Enzyme Kinetic Automata, Covalent Bonding & Nucleic Base Pairing for Idris 2**

`FinSc-Chemistry` forms **Layer 5b** of the 10-layer constructive non-linear multiset science framework. It formalizes molecular bonding matrices, covalent bond formations, secondary hydrogen bonding networks, Watson-Crick base pairing (A-T, G-C), macromolecular chirality (L-amino acids), Michaelis-Menten kinetic automata, and nucleosynthesis balance networks.

---

## 📦 Core Library Architecture & Modules

### 1. `Compound.MolecularBonding` & `Compound.Biomolecules`
- **Covalent Molecular Bonding:** Covalent bond matrices mapping valence electron multisets to stable molecular orbitals.
- **Biomolecules:** Core biomolecular multiset definitions (monosaccharides, amino acids, nucleotides, lipids).

### 2. `Compound.HydrogenBonding` & `Math.HydrogenBonding`
- **Secondary H-Bonding Networks:** Directional hydrogen bonding matrices, dipole-dipole water clusters, and hydration shell dynamics.

### 3. `Compound.WatsonCrickBasePairing` & `Math.NucleicAcidBasePairing`
- **Watson-Crick Base Pairing Rules:** Exact multiset accounting for Adenine-Thymine (A-T, 2 H-bonds) and Guanine-Cytosine (G-C, 3 H-bonds) pairing, double helix thermodynamic stability.

### 4. `Compound.MacromolecularChirality`, `Compound.MacromolecularAssembly`, `Compound.MolecularAggregation`
- **Macromolecular Chirality:** L-amino acid vs D-sugar homochirality selection matrices.
- **Protein Folding & Assembly:** Protein tertiary folding assembly matrices and supramolecular aggregation kinetics.

### 5. `Compound.ChemistryScaleTransforms`
- **Atomic-to-Molecular Scale Pipeline:** Scale transformation `ChemistryScaleTransforms` mapping atomic nuclei (`NucleusToken`) and electron shells to complex biomolecules (`MoleculeToken`).

### 6. `Math.PeakBindingEnergy`, `Math.PlasmaRecombination`, `Math.TripleAlphaNucleosynthesis`
- **Covalent Binding Energy Peaks:** Peak binding energy thresholds and nuclear plasma electron recombination kinetics.

---

## 🚀 Building & Installing

```bash
idris2 --build FinSc-Chemistry.ipkg
idris2 --install FinSc-Chemistry.ipkg
```

---

## 🔬 Architectural Principles

- **Total Constructivism:** Enforces `%default total` across all molecular chemistry modules.
- **Exact Hydrogen Bond Accounting:** Discrete multiset counting for A-T (2 H-bonds) and G-C (3 H-bonds) pairing.
- **Zero Floating-Point Drift:** Pure integer and rational multiset cross-multiplication over molecular kinetic states.
