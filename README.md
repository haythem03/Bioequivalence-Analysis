# Bioequivalence-Analysis
An R Shiny application for end-to-end drug development simulations, integrating molecular property prediction with population pharmacokinetic (popPK) and bioequivalence (BE) modeling.

This platform allows pharmaceutical scientists and students to:
* **Analyze molecular structures** from a simple SMILES string.
* **Predict PK parameters** based on a molecule's physicochemical properties.
* **Simulate different formulations** (e.g., immediate-release, extended-release, fed/fasted state).
* **Run a virtual bioequivalence (BE) study** to compare formulations.
* **Visualize and analyze results** with interactive plots and data tables.

---

### How to Install and Run

This project requires both R and Python to be installed on your system.

#### 1. Clone the Repository

```bash
git clone https://github.com/haythem03/Bioequivalence-Analysis.git
cd Bioequivalence-Analysis
```

#### 2. Set Up the R Environment

The R dependencies are listed in `Rscript/Dependencies.md`. To install them, simply run the installation script from your R console:

```r
source("Rscript/install_packages.R")
```
