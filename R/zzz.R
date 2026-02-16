.onAttach <- function(libname, pkgname) {
    packageStartupMessage("bdm version 1.0.0 (16-Feb-2026)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("methods", "rstan", "StanHeaders", "rlang", "dplyr", "ggplot2"),
        requireNamespace, quietly = TRUE)
  ))
}
