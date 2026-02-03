#' @title Convert a \code{bdm} object to a format suitable for the \pkg{kobe} package
#' @description
#' The \pkg{kobe} package (https://github.com/flr/kobe) can be used to produce diagnostic outputs concerning status of the stock relative to MSY-based reference points.
#' 
#' @param .Object a \code{bdm} class object
#' @param projection an optional \code{list} containing results from a call to \code{\link{project}}
#' @param what one or both of "samples" or "summary"
#' @param prob probabilities used for calculation of summary statistics per year
#' @param year year or years included in output
#' @param ... arguments to generic function (not used)
#' @seealso \href{https://github.com/flr/kobe/blob/master/R/kobe-bdm.R}{\code{kobe-bdm}} 
#' @examples
#' # see vignette
#' \dontrun{
#' vignette('bdm-examples')
#' }
#' 
#' @include bdm-class.R dot-array2dfr.R
#' @importFrom stats median
#' @importFrom dplyr group_by summarise %>% bind_rows
#' @export
#{{{ as.kobe functions
setGeneric("as.kobe", function(.Object, ...) standardGeneric("as.kobe"))
#' @rdname as.kobe
#{{ convert bdm object into kobe dataframe with or without projections
setMethod("as.kobe", signature = "bdm", definition = function(.Object, projection,
                                                  what = c("samples", "summary")[1],
                                                  prob = c(0.75, 0.5, 0.25), year = NULL) {
      
    out <- list()
      
    if (missing(projection)) {
      
      res <- .read_bdm(.Object)
      res <- .io_bdm(res, what = what, prob = prob, year = year)
      
      out <- res
      
    } else {

      res <- .read_bdm_projection(.Object, projection)
      
      res <- lapply(res, FUN = .io_bdm, what = what, prob = prob, year = year)
    
      out$samples <- bind_rows(lapply(res, function(x) x$samples))
      out$summary <- bind_rows(lapply(res, function(x) x$summary), .id = "projection_value")
    }
    
    if (length(what) == 1) {
       return(out[[what]])
    } else {
       return(out[what])
    }
  })
#{ read MCMC stanfit output and return data.frame with headers: iter,year,stock,harvest,bmsy,fmsy 
.read_bdm <- function(.Object){
  
  years <- .Object@data$time
  niter <- .Object@nsamples
  iters <- 1:niter
  
  res <- .Object@trace$biomass
  dimnames(res) <- list(iter = iters, year = years)
  
  res <- .array2dfr(res, value_to = 'stock')
  res <- data.frame(res, harvest = .array2dfr(.Object@trace$harvest_rate)$value)
  
  bmsy <- .Object@trace$biomass_at_msy
  fmsy <- .Object@trace$harvest_rate_at_msy
  
  ref <- data.frame(iter = iters, bmsy = bmsy, fmsy = fmsy)
  
  res <- merge(res, ref, by = "iter")
  
  res$stock   <- res$stock / res$bmsy
  res$harvest <- res$harvest / res$fmsy
  res$year    <- as.numeric(res$year)
  
  if (length(.Object@run) > 0) {
    run <- .Object@run
    res <- data.frame(run = run, res)
  }

  return(res)
}
#}
#{ read MCMC stanfit output and projection (list)
.read_bdm_projection <- function(.Object, projection){
  
  years <- projection$time
  niter <- projection$nsamples
  iters <- 1:niter
  
  nscenario <- length(projection$scenarios)
  
  res <- list()
  for (s in 1:nscenario) {
  
	  res_sc <- projection$biomass[,,s]
	  dimnames(res_sc) <- list(iter = iters, year = years)
	  
	  res_sc <- .array2dfr(res_sc, value_to = 'stock')
	  res_sc <- data.frame(res_sc,harvest = .array2dfr(projection$harvest_rate[,,s])$value)
	  
	  bmsy <- .Object@trace$biomass_at_msy
	  fmsy <- .Object@trace$harvest_rate_at_msy
	  
	  ref <- data.frame(iter = iters,bmsy = bmsy,fmsy = fmsy)
	  
	  res_sc <- merge(res_sc, ref, by = "iter")
	  
	  res_sc$stock   <- res_sc$stock / res_sc$bmsy
	  res_sc$harvest <- res_sc$harvest / res_sc$fmsy
	  
	  res_sc$projection_value <- projection$scenarios[s]
    
	  if (length(.Object@run) > 0) {
	    run    <- .Object@run
	    res_sc <- data.frame(run = run, res_sc)
	  }
  
	  res[[s]] <- res_sc
  }
  
  names(res) <- projection$scenarios
  
  return(res)
}
#}
#{ formatting
.io_bdm <- function(res, prob, what, year){
    
    if (!is.null(year)) {
       res <- res[res$year %in% year,]
    }
    
    kobe_quadrant <- function(stock, harvest) {
            
            b <-     pmax(pmin(as.integer(stock),  1),0)
            f <- 1 - pmax(pmin(as.integer(harvest),1),0)
            p <- f * b
            collapsed <- (1 - b) * (1 - f)
            
            red    <- collapsed
            green  <- p
            yellow <- 1 - p - collapsed
            
            overFished  <- 1 - b
            overFishing <- 1 - f  
            
            data.frame(red = red, green = green, yellow = yellow, overFished = overFished, overFishing = overFishing)
        }
    
    res <- data.frame(res, kobe_quadrant(as.numeric(res$stock), as.numeric(res$harvest)))
        
    res_summary <- res %>% group_by(.data$year) %>% summarise(stock = median(.data$stock,       na.rm = TRUE),
                                                     harvest     = median(.data$harvest,     na.rm = TRUE),
                                                     red         = mean(.data$red,         na.rm = TRUE),
                                                     yellow      = mean(.data$yellow,      na.rm = TRUE),
                                                     green       = mean(.data$green,       na.rm = TRUE),
                                                     overFished  = mean(.data$overFished,  na.rm = TRUE),
                                                     overFishing = mean(.data$overFishing, na.rm = TRUE))
    
    return(list(samples = res, summary = res_summary))
    

}
#}
#}}
#}}}
