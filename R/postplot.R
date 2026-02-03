#' @title Plot posterior updates from \code{bdm} model fit
#' @description
#' Plots the posterior updates for \eqn{r} and \eqn{ln(K)}.
#' 
#' Depletion is measured as the biomass over the carrying capacity, harvest rate is the catch over the estimated biomass, and surplus production is the production function multiplied by the process error residual. 
#' 
#' @param object \code{bdm} class object.
#' @param labels character vector of labels for each model run
#' @param type character vector of plot type. Must be one or more of \code{'histogram'} or \code{'point'}.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @import ggplot2
#' 
#' @export
postplot <- function(object, ...) UseMethod("postplot")
#'
#' @rdname postplot
#' @export
postplot.bdm <- function(object, ..., type = "histogram", labels = character()) {
  
    pars = c("r", "rPrior", "logK", "logKprior")
    
    y <- c(object, list(...))
    
    is.labelled <- ifelse(length(labels) > 0, TRUE, FALSE)
    
    if (is.labelled & length(y) != length(labels)) {
      stop("'labels' vector length does not match number of models")  
    }
    
    if (type == "histogram") {
      
      dfr <- data.frame(iter = integer(), value = numeric(), label = character(), par = character(), run = character())
      for (i in 1:length(y)) {
          x <- y[[i]]
          
          if (is.labelled) {
              x@run <- labels[i]
          }
          if (length(x@run) == 0) {
              warning("'run' unspecified") 
          }
          
          for (par in pars) {
            
              par_dfr <- data.frame(iter = 1:x@nsamples, 
                                    value = x@trace[[par]], 
                                    label = ifelse(par %in% c("r", "logK"), "Posterior", "Prior"), 
                                    par = ifelse(par %in% c("r", "rPrior"), "r", "logK"), 
                                    run = ifelse(length(x@run) == 0, "", x@run))
              
              dfr <- rbind(dfr, par_dfr)
          }
      }
      
      if (is.labelled) {
        dfr$run <- factor(dfr$run)
        dfr$run <- factor(dfr$run, levels = levels(dfr$run)[match(levels(dfr$run), labels)])
      }
      
      gg <- ggplot(dfr) + geom_density(aes(x = .data$value, fill = .data$label), alpha = 0.4) + theme_bw(base_size = 16) + labs(xlab = "", ylab = "", fill = "")
      
      if (length(y) > 1) {
          gg <- gg + facet_grid(.data$run ~ .data$par, scales = "free")
      } else {
          gg <- gg + facet_wrap(~ .data$par, scales = "free")
      }
    
    } else {
      
      dfr <- data.frame(iter = integer(), value_r = numeric(), value_logK = numeric(), label = character(), run = character())
      for (i in 1:length(y)) {
          x <- y[[i]]
          
          if (is.labelled) {
            x@run <- labels[i]
          }
          if (length(x@run) == 0) {
              warning("'run' unspecified") 
          }
          
          par_dfr <- data.frame(iter = 1:x@nsamples, 
                                value_r = x@trace[["rPrior"]], 
                                value_logK = x@trace[["logKprior"]], 
                                label = "Prior", 
                                run = ifelse(length(x@run) == 0, "", x@run))
          
          dfr <- rbind(dfr, par_dfr)
          
          par_dfr <- data.frame(iter = 1:x@nsamples, 
                                value_r = x@trace[["r"]], 
                                value_logK = x@trace[["logK"]], 
                                label = "Posterior", 
                                run = ifelse(length(x@run) == 0, "", x@run))
          
          dfr <- rbind(dfr, par_dfr)
      }
      
      if (is.labelled) {
        dfr$run <- factor(dfr$run)
        dfr$run <- factor(dfr$run, levels = levels(dfr$run)[match(levels(dfr$run), labels)])
      }
      
      gg <- ggplot(dfr) + 
        geom_point(aes(x = .data$value_r, y = .data$value_logK, col = .data$label), alpha = 0.4, size = 2) +
        labs(x = expression(r), y = expression(ln(K)), col = "Samples") +
        theme_bw(base_size = 16)
      
      if (length(y) > 1) {
          gg <- gg + facet_wrap(~ .data$run)
      }
    }
    return(gg)
}
#}}}
