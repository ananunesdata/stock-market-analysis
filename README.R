# Market Regimes in the Euro Area — A Non-Homogeneous Hidden Markov Model for the MSCI Europe

Financial, economic, and social dynamics behind Bull and Bear regimes, estimated with a Non-Homogeneous Hidden Markov Model (HMM-NH) and interpreted with Shapley values.

> Undergraduate capstone project — Bachelor’s degree in Applied Mathematics and Computing, Instituto Superior Técnico, Lisbon. Supervised by Prof. Maria do Rosário Oliveira and Prof. Pedro Ferreira dos Santos.

---
  
  ## 📄 Overview
  
  Most regime-switching literature in finance relies almost exclusively on financial covariates. This project asks a broader question: **do real-economy and social indicators (unemployment, inequality, corruption, crime, emissions) help explain when the Euro Area equity market shifts from a Bull to a Bear regime, and vice versa?**
  
  Using monthly log-returns of the **MSCI Europe index (1997–2024)**, the project:
  
  1. Builds a dataset combining financial, macroeconomic, and social/structural indicators for the Euro Area.
2. Estimates a **two-state Non-Homogeneous Hidden Markov Model (HMM-NH)**, where transition probabilities and the emission mean are both driven by external, time-varying covariates.
3. Solves the model with the **Baum-Welch algorithm** (Forward-Backward + Newton-Raphson for the logistic transition step) and decodes the most likely regime path with the **Viterbi algorithm**.
4. Uses **Shapley values** to exactly decompose the contribution of each covariate to the estimated transition probabilities, guiding a principled variable-selection process (AIC/BIC) that avoids overfitting from correlated covariates.

The final specification identifies Bear regimes that align closely with well-known Euro Area shocks: the dot-com crash (2000–2003), the Global Financial Crisis (2008), the European sovereign debt crisis (2011–2012), and the COVID-19 shock (2020).



---
  
  ## 🗂️ Repository structure
  
  ```
.
├── R/
│   ├── 00_packages_and_helper_functions.R    # dependencies + shared helper functions
│   ├── 01_data.R                             # raw data import
│   ├── 02_data_handling.R                    # cleaning, merging, feature engineering
│   ├── 03_plots.R                            # exploratory data analysis & figures
│   ├── 04_tests.R                            # stationarity / ADF / KPSS / cross-correlation tests
│   ├── 05_HMM.R                              # HMM-NH: Baum-Welch, Viterbi, model specifications
│   └── 06_shapley_values.R                   # exact Shapley decomposition of transition probabilities
├── data/                                     # raw & processed data (see note below)
├── figures/                                  # exported plots used in the report
├── papper.pdf                                # full written report
└── README.md
```

> **Note on `data/`:** raw series come from third-party sources (Curvo, Eurostat, ECB, FRED, OECD, UNODC, V-Dem/Our World in Data, CountryEconomy.com, ParlGov — see full list in the report's references). Large or license-restricted raw files are not committed; `01_data.R` documents exactly where to download each series from.

---

## 🧠 Methodology in brief

- **Model:** two-state HMM-NH, `λ_t = (π, A_t, B_t)`, states = {Bull, Bear}.
- **Transitions:** multinomial logistic link on time + covariates, `A_t`.
- **Emissions:** Gaussian, mean modeled as a linear function of a time trend + covariates, `B_t`.
- **Estimation:** EM (Baum-Welch); closed-form updates for `π`, emission mean/variance (weighted least squares); Newton-Raphson (ridge-regularized, with step-halving) for the transition logistic coefficients.
- **Decoding:** Viterbi algorithm for the globally most likely regime path.
- **Interpretability:** exact Shapley values (game-theoretic feature attribution) applied to the transition probabilities, used both to rank covariates by importance and to select a parsimonious final model via AIC/BIC.

Four specifications were compared — a full model (10 covariates), a base model (CO₂ emissions, 3-month interest rate, political corruption), and two intermediate models — with the base model selected as it minimizes AIC/BIC while avoiding the multicollinearity-driven instability of the full model.

---

## ⚙️ Requirements

- R (≥ 4.2 recommended)
- Key packages (see `R/00_packages_and_helper_functions.R` for the full list and `install.packages()` calls):
  - `tidyverse`, `lubridate` — data wrangling
  - `mFilter` — Hodrick-Prescott filter
  - `tseries`, `urca` — ADF / KPSS stationarity tests
  - custom HMM-NH implementation (Baum-Welch, Viterbi, Newton-Raphson) — no external HMM package used, implemented from scratch in `05_HMM.R`
  - custom exact Shapley value implementation in `06_shapley_values.R`

## ▶️ How to run

```r
# from the repository root
source("R/00_packages_and_helper_functions.R")
source("R/01_data.R")
source("R/02_data_handling.R")
source("R/03_plots.R")
source("R/04_tests.R")
source("R/05_HMM.R")
source("R/06_shapley_values.R")
```

Each script is meant to be run in order; later scripts assume objects created by earlier ones are in the environment.

---

## 📊 Key results

- Financial, real-economy, and social/structural covariates jointly help explain Euro Area market regime transitions — but with strong multicollinearity risk when all ten candidate covariates are included at once (a 10-covariate model overfits and produces an unstable, fragmented regime sequence).
- Shapley decomposition ranks **crime rate, CO₂ emissions, and short-term interest rates** as the covariates with the largest average impact on transition probabilities, with **political corruption** and **GNI** contributing moderately.
- The selected **base model** (emissions, 3-month interest rate, corruption) achieves the best AIC/BIC trade-off and produces Bear regimes that line up with real historical crises, validating the approach economically as well as statistically.


## Report

The final report is written in Portuguese.
