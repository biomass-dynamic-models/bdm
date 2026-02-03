#' @title Traceplot for MCMC output
#' @aliases traceplot-bdm
#' @description
#' Plots the trace outputs from an MCMC run implemented using \code{\link{sampler}}.
#' 
#' This function uses the S4 generic \code{\link[rstan]{traceplot}} provided by \pkg{rstan}.
#' 
#' @param object a \code{bdm} object
#' @param pars parameters to be plotted
#' @param inc_warmup logical value indicating whether the warmup values should be included
#' @param nrow passed to \code{\link[ggplot2]{facet_wrap}}
#' @param ncol passed to \code{\link[ggplot2]{facet_wrap}}
#' @param ... optional arguments to \code{\link[ggplot2]{geom_line}}
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include dot-array2dfr.R
#' 
#' @import ggplot2
#' @import rstan
#' 
#' @export
setMethod("traceplot", signature = "bdm", function(object, pars = c('r', 'logK', 'lp__'), inc_warmup = TRUE, nrow = NULL, ncol = NULL, ...) {
  
  dfr <- data.frame(variable = NULL, chain = NULL, value = NULL)
  
  # extract paramater values from object@trace_array
  for (par in pars) {
    m <- regexpr('\\[.+\\]',par)
    if (m > 0) {
      i <- match(par, dimnames(object@trace_array)$parameters)
      if (!is.na(i)) {
        dfr_tmp <- .array2dfr(object@trace_array[,,i,drop = FALSE])
	      if (ncol(dfr_tmp) > 2) {
			    dfr <- rbind(dfr, data.frame(variable = dimnames(object@trace_array)$parameters[i], iteration = dfr_tmp$iterations, chain = dfr_tmp$chains, value = dfr_tmp$value))
		    } else {
			    dfr <- rbind(dfr, data.frame(variable = dimnames(object@trace_array)$parameters[i], iteration = 1:dim(dfr_tmp)[1], chain = '1', value = dfr_tmp$value))
		    }
	    }
    } else {
      mm <- 0
      for (parname in dimnames(object@trace_array)$parameters) {
        m  <- regexpr(par, parname)
        if (m > 0) { 
          i <- match(parname, dimnames(object@trace_array)$parameters)
          dfr_tmp <- .array2dfr(object@trace_array[,,i,drop = FALSE])
          if (ncol(dfr_tmp) > 2) {
              dfr <- rbind(dfr, data.frame(variable = parname, iteration = dfr_tmp$iterations, chain = dfr_tmp$chains, value = dfr_tmp$value)) 
          } else {
              dfr <- rbind(dfr, data.frame(variable = parname, iteration = 1:dim(dfr_tmp)[1], chain = '1', value = dfr_tmp$value)) 
          }
          mm <- mm + 1
        } else {
          if (mm > 0) break
        }
      }
    }
  }
  if (!nrow(dfr) > 0) stop('parameter not found\n')
  
  if (!inc_warmup) {
    dfr <- subset(dfr, dfr$iteration > (object@warmup / object@thin))
  }
  
  gg <- ggplot(dfr)
    
  if (inc_warmup) {
    gg <- gg + geom_vline(xintercept = (object@warmup / object@thin), linetype = 'longdash',col = 'grey50')
  }
  
  gg <- gg + geom_line(aes(x = as.numeric(.data$iteration), y = .data$value, col = .data$chain), ...) + 
    facet_wrap(~.data$variable, scales = 'free_y', ncol = ncol, nrow = nrow) +
    xlab('Posterior sample') +
    ylab('Parameter value') + guides(col = "none")
  
  return(gg)
  
})
#}}}
