#' @title Plot the \code{bdm} model fit
#' @description
#' Plots the estimated abundance index dynamics over time against the observed values
#' 
#' @param object a fitted \code{bdm} class object.
#' @param labels character vector of labels for each index.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include dot-array2dfr.R
#' 
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats quantile median
#' @export
idxplot <- function(object, ...) UseMethod("idxplot")
#'
#' @rdname idxplot
#' @export
idxplot.bdm <- function(object, labels, ...) {
    
    nidx     <- object@data[['I']]
    time     <- object@data[['time']]
    nsamples <- object@nsamples
    
    if (missing(labels)) {
        labels <- paste('index:', 1:nidx, sep = '')
	}
    
    #######################################################
    # code for extraction of iterations from object@trace #
    # (list of parameter arrays with all chains combined) #
    #######################################################
    dfr <- data.frame(iter = integer(),time = integer(),value = numeric(),label = character())
    idx_arr <- object@trace[['predicted_index']]
    dimnames(idx_arr) <- list(iter = 1:nsamples, time = time, index = 1:nidx)
    for (i in 1:nidx) {
        
        idx_dfr <- .array2dfr(idx_arr[,,i])
        idx_dfr <- data.frame(idx_dfr, label = labels[i])
        
        dfr <- rbind(dfr, idx_dfr)
    }
    
    gg <- ggplot() + 
        stat_summary(data = dfr, aes(.data$time, .data$value),fun.min = function(x) quantile(x, 0.025),fun.max = function(x) quantile(x, 0.975),geom = 'ribbon',alpha = 0.3) +
		stat_summary(data = dfr, aes(.data$time, .data$value),fun = function(x) median(x), geom = 'line', linewidth = 1) +
        stat_summary(data = dfr, aes(.data$time, .data$value),fun = function(x) mean(x),   geom = 'line', linewidth = 1, linetype = "dashed") +
        labs(x = 'Time',y = 'Predicted Index')
    
    dfr_empirical <- object@data$index
    dimnames(dfr_empirical) <- list(time = object@data$time, label = labels)
    dfr_empirical[dfr_empirical < 0] <- NA_real_
    dfr_empirical <- .array2dfr(dfr_empirical)
    
    gg <- gg + geom_point(data = dfr_empirical, aes(.data$time, .data$value, col = .data$label), size = 4) + 
        geom_line(data = dfr_empirical, aes(.data$time, .data$value, col = .data$label), linewidth = 1) + guides(col = "none")
    
    if (nidx > 1) {
        gg <- gg + facet_grid(.data$label~.)
	}
    
    return(gg)
}
#}}}
