#' @title Convert a \code{stanfit} object into a \code{bdm} object
#' @description
#' Useful for producing plots with the \pkg{bdm} package functions such as \code{\link{histplot}}
#' 
#' @param object \code{\link[rstan]{stanfit}} class object.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{bdm} object containing MCMC outputs
#' 
#' @include bdm.R
#' 
#' @export
as.bdm <- function(object, ...) UseMethod("as.bdm")
#' @rdname as.bdm
#' @export
as.bdm.stanfit <- function(object, ...) {
    
    bdm_object <- bdm()
    
    bdm_object@thin        <- object@sim$thin
    bdm_object@warmup      <- object@sim$warmup
    bdm_object@chains	   <- object@sim$chains
    bdm_object@nsamples    <- ((object@sim$iter - object@sim$warmup) * object@sim$chains) / object@sim$thin
    
    bdm_object@trace       <- extract(object)
    bdm_object@trace_array <- extract(object, permuted = FALSE, inc_warmup = TRUE)
    
    return(bdm_object)
}
