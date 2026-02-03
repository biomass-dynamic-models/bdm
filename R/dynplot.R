#' @title Plot estimated dynamics from \code{bdm} model fit
#' @description
#' Plots the dynamics over time of the estimated biomass, depletion, harvest rate or surplus production.
#' @details
#' Depletion is measured as the biomass over the carrying capacity, harvest rate is the catch over the estimated biomass, and surplus production is the production function multiplied by the process error residual. Multiple model runs can be provided, in which case the are superimposed.
#' 
#' @param object \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Must be one or more of \code{'depletion'}, \code{'biomass'}, \code{'harvest_rate'} or \code{'surplus_production'}.
#' @param labels character vector of labels per model run
#' @param ... additional \code{bdm} class objects
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package. The plotted dynamics are summarised as the median and the 75th and 95th percentiles. The posterior mean is shown as a dashed line. 
#' @include dot-array2dfr.R
#' @import ggplot2
#' @importFrom rlang .data
#' 
#' @export
dynplot <- function(object, ...) UseMethod("dynplot")
#'
#' @rdname dynplot
#' @export
#dynplot.bdm <- function(object, pars = 'depletion', ...) {
#    
#    time <- object@data[['time']]
#    nsamples <- object@nsamples
#    
#    #######################################################
#    # code for extraction of iterations from object@trace #
#    # (list of parameter arrays with all chains combined) #
#    #######################################################
#    dfr <- data.frame(iter = integer(),time = integer(),value = numeric(),label = character())
#    for (par in pars) {
#        par_arr <- object@trace[[par]]
#        dimnames(par_arr) <- list(iter = 1:nsamples,time = time)
#        
#        par_dfr <- melt(par_arr)
#        par_dfr <- data.frame(par_dfr,label = par)
#        
#        dfr <- rbind(dfr,par_dfr)
#    }
#    
#    gg <- ggplot(dfr,aes(time,value)) + 
#        stat_summary(fun.ymin = function(x) quantile(x,0.025),fun.ymax = function(x) quantile(x,0.975),geom = 'ribbon',alpha = 0.3) +
#        stat_summary(fun.y = function(x) mean(x),geom = 'line',lwd = 1.5) +
#        labs(x = 'Time',y = 'Predicted Value') #+
#        #ggtheme()
#    
#    if (length(pars)>1)
#        gg <- gg + facet_grid(label~., scales  =  'free_y')
#    
#    return(gg)
#}

dynplot.bdm <- function(object, ..., pars = 'depletion', labels = character()) {
    
    y <- c(object, list(...))
    
    is.labelled <- ifelse(length(labels) > 0, TRUE, FALSE)
    
    if (is.labelled & length(y) != length(labels)) {
        stop("'labels' vector length does not match number of models")  
    }
    
    dfr <- data.frame(iter = integer(), time = integer(), value = numeric(), label = character(), run = character())
    for (i in 1:length(y)) {
        x <- y[[i]]
        for (par in pars) {
            par_arr <- x@trace[[par]]
            dimnames(par_arr) <- list(iter = 1:x@nsamples, time = x@data$time)
            
            if (is.labelled) {
                x@run <- labels[i]
            }
            if (length(x@run) == 0) {
                warning("'run' unspecified") 
            }
            
            par_dfr <- .array2dfr(par_arr)
            par_dfr <- data.frame(par_dfr, label = par, run = ifelse(length(x@run) == 0, "", x@run))
            
            dfr <- rbind(dfr, par_dfr)
        }
    }
    
    if (is.labelled) {
        dfr$run <- factor(dfr$run)
        dfr$run <- factor(dfr$run, levels = levels(dfr$run)[match(levels(dfr$run), labels)])
    }
    
    dfr$time <- as.numeric(dfr$time)
    
    dfr <- merge(dfr, data.frame(label = c("depletion", "biomass", "harvest_rate", "surplus_production"), label2 = c("Depletion", "Biomass", "Harvest rate", "Surplus production")))
    
    if (length(y) > 1) {
        gg <- ggplot(dfr, aes(.data$time, .data$value, col = .data$run, fill = .data$run)) + labs(x = 'Time', y = 'Predicted Value', col = 'Model\nrun', fill = 'Model\nrun')
    } else {
        gg <- ggplot(dfr, aes(.data$time, .data$value)) + labs(x = 'Time', y = 'Predicted Value')
    }
        
    gg <- gg + 
        stat_summary(fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun.min = function(x) quantile(x, 0.125), fun.max = function(x) quantile(x, 0.875), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun = function(x) median(x), geom = 'line', lwd = 1) +
        stat_summary(fun = function(x) mean(x), geom = 'line', lwd = 0.5, linetype = "dashed")
    
    if (length(pars) > 1) {
        gg <- gg + facet_grid(.data$label2~., scales  =  'free_y')
    }
    
    return(gg)
}
#}}}
