# `loadedDipole`
**Tools for Shortened Dipole and Loading Coil Design in R**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)
![Status: Experimental](https://img.shields.io/badge/status-experimental-blue)
[![pkgdown site](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://yojimbodurant.github.io/loadedDipole/)

`loadedDipole` provides numerical tools for designing and analyzing **shortened HF dipoles**, including:

- Optimizing **loading coil inductance** for a target electrical length  
- Evaluating **efficiency**, **radiation resistance**, and **losses**  
- Modeling **coil Q**, wire resistance, and distributed reactance  
- Exploring antenna configurations suitable for **portable**, **QRP**, and **restricted-space** operations

This package emerged from practical antenna design problems and includes computational methods developed with assistance from ChatGPT.

---

## Key Features

- **`optimize_inductance_for_position()`**  
  Finds the optimum loading-coil inductance and placement for a shortened dipole.

- **`estimate_efficiency()`**  
  Computes radiation efficiency based on radiation resistance, loss resistance, coil Q, and conductor losses.

- **`dipole_impedance()`** *(optional)*  
  Estimates input impedance of a shortened dipole with loading elements.

---

## Installation

### Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("YoJimboDurant/loadedDipole")
```

### Load the package:

```r
library(loadedDipole)
```

---

## Example Usage

### Optimize a loading coil for 20m band

```r
result <- optimize_inductance_for_position(
  freq = 14.076,
  total_length = 6.5,
  wire_radius = 0.001,
  coil_position_frac = 0.4
)

print(result)
```

### Estimate efficiency

```r
eff <- estimate_efficiency(
  R_r = result$R_r,
  R_loss = result$R_loss_total
)

eff
```

---

## Documentation

```r
help(package = "loadedDipole")
?optimize_inductance_for_position
?estimate_efficiency
```

---

## Development Notes

This package is experimental and under active refinement.

Planned enhancements:

- Additional optimization algorithms  
- NEC2 interface  
- Parametric efficiency plots  
- Shiny interface

---
