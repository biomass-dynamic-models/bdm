#' @title Plot posterior histograms
#' @description
#' Plots histograms of posterior samples from an MCMC chain contained within a \code{bdm} class object.
#'
#' @param object a \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Defaults to \code{pars = c('r','logK','lp__')}.
#' @param inc_warmup logical value indicating whether MCMC warmup should be included in the plot.
#' @param nrow passed to \code{\link[ggplot2]{facet_wrap}}
#' @param ncol passed to \code{\link[ggplot2]{facet_wrap}}
#' @param ... optional arguments to \code{\link[ggplot2]{geom_histogram}}
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include dot-array2dfr.R
#' 
#' @import ggplot2
#' @importFrom rlang .data
#'  
#' @export
histplot <- function(object, ...) UseMethod("histplot")
#' 
#' @rdname histplot
#' @export
histplot.bdm <- function(object, pars = c('r', 'logK', 'lp__'), inc_warmup  =  FALSE, nrow = NULL, ncol = NULL, ...) {
    
  #############################################################
  # code for extraction of iterations from object@trace_array #
  # (array with dimensions: iteration; parameter; chain)      #
  #############################################################
  dfr <- data.frame(variable = NULL,chain = NULL,value = NULL)
  
  for (par in pars) {
    m <- regexpr('\\[.+\\]',par)
    if (m > 0) {
      i <- match(par,dimnames(object@trace_array)$parameters)
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
        m  <- regexpr(par,parname)
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
  
  gg <- ggplot(dfr) + 
          geom_histogram(aes(x = .data$value, fill = .data$chain), position = 'stack', ...) + 
          facet_wrap(~.data$variable, scales = 'free_x', ncol = ncol, nrow = nrow) +
          xlab('Parameter value') +
          ylab('Sample counts') + guides(fill = "none")
  
  #######################################################
  # code for extraction of iterations from object@trace #
  # (list of parameter arrays with all chains combined) #
  #######################################################
  #loc <- array(dim = c(length(pars),2),dimnames = list(pars,c('i','j')))
  #for (par in pars) {
  #  m <- regexpr('\\[.+\\]',par)
  #  if (m>0) {
  #    loc[par,'i'] <- match(substr(par,1,m-1),names(object@trace))
  #    m <- m + 1
  #    attributes(m)$match.length <- attributes(m)$match.length - 2
  #    loc[par,'j'] <- as.numeric(regmatches(par,m))
  #  } else {
  #    loc[par,'i'] <- match(par,names(object@trace))
  #  }
  #}
  #
  #dfr <- data.frame(variable = NULL,value = NULL)
  #for (par in pars) {
  #  
  #  i <- loc[par,'i']
  #  j <- loc[par,'j']
  #  
  #  if (length(dim(object@trace[[i]]))>1) {
  #    list.tmp <- lapply(apply(object@trace[[i]],2,function(x) list(x)),unlist)
  #    if (is.na(j)) {
  #      for (j in 1:length(list.tmp)) {
  #        dfr <- rbind(dfr,data.frame(variable = paste(names(object@trace)[i],'[',j,']',sep = ''),value = list.tmp[[j]]))
  #      }
  #    } else {
  #      dfr <- rbind(dfr,data.frame(variable = paste(names(object@trace)[i],'[',j,']',sep = ''),value = list.tmp[[j]]))
  #    }
  #  } else {
  #    dfr <- rbind(dfr,data.frame(variable = names(object@trace)[i],value = object@trace[[i]]))
  #  }
  #}
  #
  #gg <- ggplot(dfr) + 
  #        geom_histogram(aes(x = value),position = 'identity') + 
  #        facet_wrap(~variable,scales = 'free_x') +
  #        xlab('Parameter value') +
  #        ylab('Sample counts') + ggtheme()
  
  return(gg)
}
#}}}
