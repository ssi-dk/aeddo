# aeddo 0.1.1

## Patch

* Transferring maintainership of the R package to Lasse Engbo Christiansen.

# aeddo 0.1.0

## Features

- Added a new function `nll_poisson_gamma()` for computing the negative log-likelihood of a Poisson-Gamma model. This function is essential for parameter estimation in the `aeddo()` algorithm.
- Introducing the `aeddo()` function for Automated and Early Detection of Disease Outbreaks. This innovative algorithm utilizes hierarchical models to infer one-step ahead random effects, providing an effective tool for identifying and responding to disease outbreaks in time series data. The function includes various parameters for customization, such as rolling window size, outbreak detection threshold, and optimization method.
