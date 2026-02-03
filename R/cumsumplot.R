#' @title Plot cumulative sum of MCMC chain
#' @description
#' Plots the cumulative sum of ordered posterior samples from an MCMC chain contained within a \code{bdm} class object.
#' 
#' @param object \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Defaults to \code{pars = c('r','logK','lp__')}.
#' @param inc_warmup logical value indicating whether MCMC warmup should be included in the plot.
#' @param nrow passed to \code{\link[ggplot2]{facet_wrap}}
#' @param ncol passed to \code{\link[ggplot2]{facet_wrap}}
#' @param ... optional arguments to \code{\link[ggplot2]{geom_line}}
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include dot-array2dfr.R
#' 
#' @import ggplot2
#' @importFrom dplyr reframe group_by %>%
#' @importFrom rlang .data
#' @export
cumsumplot <- function(object, ...) UseMethod("cumsumplot")
#' 
#' @rdname cumsumplot
#' @export
cumsumplot.bdm <- function(object, pars = c('r', 'logK', 'lp__'), inc_warmup = FALSE, nrow = NULL, ncol = NULL, ...) {
    
    #############################################################
    # code for extraction of iterations from object@trace_array #
    # (array with dimensions: iteration; parameter; chain)      #
    #############################################################
    dfr <- data.frame(variable = NULL, chain = NULL, value = NULL)
    
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
                    i <- match(parname,dimnames(object@trace_array)$parameters)
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
    
    dfr <- dfr %>% group_by(.data$variable, .data$chain) %>% reframe(value = .data$value[order(.data$value)], cumsum  =  (1:length(.data$chain)) / length(.data$chain))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(x = .data$value, y = .data$cumsum, col = .data$chain), linewidth = 1, ...) + 
        facet_wrap(~.data$variable, scales = 'free_x', ncol = ncol, nrow = nrow) +
        xlab('Parameter value') +
        ylab('Cumulative posterior sum') + guides(col = "none")
    
    return(gg)
}
#}}}
