Bayesian biomass dynamic model
==============================

<!-- badges: start -->
[![R-CMD-check](https://github.com/biomass-dynamic-models/bdm/actions/workflows/check-release.yaml/badge.svg)](https://github.com/biomass-dynamic-models/bdm/actions/workflows/check-release.yaml)
<!-- badges: end -->

Instructions
=============
First, install the `rstan` and `remotes` packages from CRAN:

    # Install packages
    install.packages("remotes")
    install.packages("rstan")

Then install `bdm` directly from GitHub:

    # remotes command to get bdm from GitHub
    remotes::install_github("biomass-dynamic-models/bdm") 

The life history module `lhm` is required to estimate the intrinsic growth rate:

    # remotes command to get lhm from GitHub
    remotes::install_github("biomass-dynamic-models/lhm") 

