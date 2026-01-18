<div align="center">

# 💊 Bioequivalence Analysis & Simulation

**An integrated R Shiny platform for end-to-end drug development simulations.**

[![R](https://img.shields.io/badge/R-4.0%2B-blue?style=for-the-badge&logo=r&logoColor=white)](https://cran.r-project.org/)
[![Shiny](https://img.shields.io/badge/Framework-Shiny-blueviolet?style=for-the-badge&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)
[![Python](https://img.shields.io/badge/Python-3.8%2B-yellow?style=for-the-badge&logo=python&logoColor=white)](https://www.python.org/)
[![License](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)](LICENSE)

[Overview](#-overview) •
[Features](#-key-features) •
[Installation](#-installation) •
[Usage](#-usage) •
[Contributing](#-contributing)

</div>

---

## 🧭 Overview

**Bioequivalence-Analysis** is a powerful R Shiny application designed to bridge the gap between molecular chemistry and clinical pharmacology.

It serves as a comprehensive simulation sandbox for pharmaceutical scientists and researchers, enabling the integration of **molecular property prediction**, **population pharmacokinetics (popPK)**, and **virtual bioequivalence (BE) trials**. By simulating the journey of a drug from structure to subject, this tool accelerates decision-making in formulation development.

### 🎯 Key Goals
* **Predict** how chemical structure influences biological behavior.
* **Simulate** clinical scenarios (Fed vs. Fasted, IR vs. ER).
* **Evaluate** bioequivalence between test and reference formulations in silico.

---

## 🚀 Key Features

| Feature | Description |
| :--- | :--- |
| **🧪 Molecular Analysis** | Instantly analyze molecular structures and physicochemical properties from a simple **SMILES** string. |
| **📈 PK Prediction** | Predict key Pharmacokinetic (PK) parameters based on calculated molecular descriptors. |
| **⚙️ Formulation Sim** | Model various scenarios, including **Immediate-Release (IR)** vs. **Extended-Release (ER)** and food effects. |
| **👥 Virtual Trials** | Run full-scale **Bioequivalence (BE) studies** on virtual populations to compare Test vs. Reference drugs. |
| **📊 Interactive Viz** | Visualize concentration-time profiles, statistical distributions, and BE metrics with dynamic plots. |

---

## 🛠️ Installation

This project operates as a hybrid application, leveraging **R** for the interface/statistics and **Python** for cheminformatics.

### Prerequisites
* **R:** [Download from CRAN](https://cran.r-project.org/)
* **Python:** [Download from python.org](https://www.python.org/downloads/)
* **RStudio:** [Download IDE](https://posit.co/download/rstudio-desktop/) (Highly Recommended)

### Step-by-Step Guide

#### 1. Clone the Repository
```bash
git clone [https://github.com/haythem03/Bioequivalence-Analysis.git](https://github.com/haythem03/Bioequivalence-Analysis.git)
cd Bioequivalence-Analysis
```
#### 2\. Set Up the Python Environment

The app uses `reticulate` to interface with Python libraries for molecular handling. Install the required packages via pip:

Bash

  

pip install rdkit-pypi pandas numpy

#### 3\. Set Up the R Environment

We have streamlined the R dependency installation. Navigate to the script folder and run the installer:

Bash

  

\# Navigate to the Rscript folder
cd Rscript

# Run the installation script
Rscript install\_packages.R

> _Alternatively, open_ `_Rscript/install_packages.R_` _in RStudio and run the script manually._

## 💻 Usage

Once your environment is configured, launching the simulation dashboard is simple.

1.  Open RStudio or your R console.
2.  Ensure you are in the project root directory (`Bioequivalence-Analysis/`).
3.  Run the following command:

R

  

shiny::runApp()

The application will automatically launch in your default web browser.

## 🤝 Contributing

Contributions make the open-source community an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1.  **Fork the Project**
2.  **Create your Feature Branch** (`git checkout -b feature/AmazingFeature`)
3.  **Commit your Changes** (`git commit -m 'Add some AmazingFeature'`)
4.  **Push to the Branch** (`git push origin feature/AmazingFeature`)
5.  **Open a Pull Request**

## 📄 License

Distributed under the **MIT License**. See `LICENSE` for more information.
