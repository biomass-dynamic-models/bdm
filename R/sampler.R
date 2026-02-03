#' @title Fit \code{bdm} model
#' @description
#' Execute a Bayesian model fit using \pkg{rstan}.
#' 
#' By default a Bayesian fit is executed through a call to \code{\link[rstan:sampling]{sampling}}, which implements an MCMC algorithm. Default values for \code{chains}, \code{iter}, \code{warmup} and \code{thin} follow those for \pkg{rstan}.
#' 
#' The \code{init} argument can be a \code{list}, \code{function} or \code{character} string. If it is a function then it should take no arguments and return a named list of intial values for the estimated parameters. Alternatively the list can be specified directly.
#' This behaviour matches that for \code{\link[rstan:sampling]{sampling}}. If a character string is supplied it should be either \code{'random'} or \code{'fixed'}. 
#' If the model is the default model and \code{init = 'fixed'} then sensible starting values for \code{r}, \code{logK} and \code{x} are produced using \code{\link{getr}}, \code{\link{getlogK}} and \code{\link{getx}}. 
#' If the model is the default model and \code{init = 'random'} then sensible starting values are obtained by sampling from the priors for \code{r, logK, x}.
#' If the model is not the default model, then the user should specify a function or list, otherwise starting values will be randomly generated.
#' 
#' @param object a \code{bdm} model object
#' @param data a \code{list} object containing the model inputs
#' @param run optional character vector to label the run
#' @param init an initialisation \code{list}, \code{function} or \code{character} string
#' @param chains number of MCMC chains
#' @param iter number of iterations per chain
#' @param warmup number of iterations to be discarded
#' @param thin sampling interval from chains
#' @param ... further arguments to \code{\link[rstan:sampling]{sampling}}
#' 
#' @return Returns a \code{bdm} object containing posterior samples contained in \code{object@@trace}.
#' 
#' @examples
#' # get some data
#' data(albio)
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = albio$year)
#' 
#' # initialize and fit default model
#' \dontrun{
#' mdl <- bdm()
#' mdl <- compiler(mdl)
#' mdl <- sampler(mdl, dat)
#' }
#' 
#' @include bdm.R getparams.R getr.R getx.R getlogK.R
#' 
#' @import methods
#' @import rstan
#' @importFrom stats rlnorm runif rbeta
#' 
#' @export
setGeneric("sampler", function(object, ...) standardGeneric("sampler"))
#'
#' @rdname sampler
setMethod("sampler", signature = "bdm", definition = function(object, data = list(), run = character(), init, chains, iter, warmup, thin, ...) {
    
    # initial assignments
    object@data <- data
    object@run  <- run
    
    # non-default sampling dimensions
    if (!missing(iter)) {
      object@iter <- iter
      if (missing(warmup)) {
          warmup <- floor(iter / 2)
      }
    }
    if (!missing(chains)) object@chains <- chains
    if (!missing(thin))   object@thin   <- thin
    if (!missing(warmup)) {
      if (warmup >= object@iter) {
          stop('warmup must be < iter\n')
      }
      object@warmup <- warmup
    }
    
    # extract estimated parameters
    pars <- getparams(object)
    pars <- setdiff(pars, names(object@data))
    
    # record
    object@pars <- pars
    
    # check for default parameters
    is_default <- setequal(pars, c('r', 'logK', 'x'))
    
    # pars for which we can generate starting values
    pars <- intersect(pars, c('r', 'logK', 'x'))
    
    # create initial value function
    if (missing(init)) {
        
        INITIAL_VALUES <- FALSE
    } else {
        
        if (is.character(init) & length(pars) > 0) {
            
            if (init == "random" & !is_default) {
                
                init <- "fixed"
                warning("'init = random' only allowed for default parameter set: changed to 'init = fixed' for ", paste0(pars, collpase = ", "))
            }
            
            if (init == "fixed") {
                
                init_list <- list()
                
                if (isTRUE("r"    %in% pars)) init_list$r    <- getr(object)[['E[r]']]
                if (isTRUE("logK" %in% pars)) init_list$logK <- getlogK(object)[['E[logK]']]
                if (isTRUE("x"    %in% pars)) init_list$x    <- getx(object)[['E[x]']]
                
                init <- function() {
                    init_list
                }
                
                INITIAL_VALUES <- TRUE
                
            } else if (init == "random") {
                
                init_r    <- getr(object)
                init_logK <- getlogK(object)
                init_x    <- getx(object)
                
                init <- function() {
                    list(r = rlnorm(1, init_r[['E[log(r)]']], init_r[['SD[log(r)]']]), logK = runif(1, init_logK[['min[logK]']], init_logK[['max[logK]']]), x = rbeta(length(init_x[['E[x]']]), 2, 2))
                }
                
                INITIAL_VALUES <- TRUE
            } 
        } else { 
            
            if (is.list(init)) {
                
                init <- function() init
                
                INITIAL_VALUES <- TRUE
                
            } else {
                
                if (is.function(init)) {
                    
                    INITIAL_VALUES <- TRUE
                    
                } else {
                    
                    warning("'init' argument is ignored (invalid format)")
                    
                    INITIAL_VALUES <- FALSE
                }
            }
        }
    }
    
    # number of posterior samples
    object@nsamples <- ((object@iter - object@warmup) * object@chains) / object@thin
    
    # mcmc-sampling using rstan
    if (INITIAL_VALUES) {
        stanfit_object <- suppressWarnings(sampling(object, data = object@data, init = init, iter = object@iter, chains = object@chains, warmup = object@warmup, thin = object@thin, ...))
    } else {
        stanfit_object <- suppressWarnings(sampling(object, data = object@data, iter = object@iter, chains = object@chains, warmup = object@warmup, thin = object@thin, ...))
    }
    # record initial values for each chain
    object@inits <- stanfit_object@inits
    
    # extract traces
    object@trace       <- extract(stanfit_object)
    object@trace_array <- extract(stanfit_object, permuted = FALSE, inc_warmup = TRUE)
    
    # return
    return(object)
})
