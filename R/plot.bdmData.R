#' @title Plot index and harvest data
#' @description
#' This function provides a \code{plot} method for the \code{bdmData} object class. It returns a \code{ggplot} object that can be assigned and manipulated using functions provided by \pkg{ggplot2}.
#' 
#' @param x an \code{\link{bdmData}} object class
#' @param ... required by the generic function
#' 
#' @examples
#' # load Indian Ocean albacore data
#' data(albio)
#' 
#' # create bdmData object
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = albio$year)
#' 
#' # plot
#' plot(dat)
#' 
#' @import ggplot2
#' @importFrom rlang .data
#' @method plot bdmData
#' @export
plot.bdmData <- function(x, ...)
{

	time    <- as.numeric(x[['time']])
	harvest <- x[['harvest']]
    index   <- x[['index']]
    
    index[index < 0] <- NA_real_
    
    #plt <- rainbow(dim(index)[2])
    
    # color blind palette
    plt <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
	
    dfr <- data.frame(time = time, value = harvest, label = 'Harvest')
	gg <- ggplot() + 
        geom_col(data = dfr, aes(.data$time, .data$value), fill = "#999999")
	
    for (i in 1:dim(index)[2]) {
        
        dfr <- data.frame(time = time, value = index[,i], label = 'Index')
        dfr <- subset(dfr, !is.na(dfr$value))
        
        gg <- gg + 
            geom_point(data = dfr, aes(.data$time, .data$value), col = plt[i], size = 2) +
            geom_line(data = dfr, aes(.data$time, .data$value), col = plt[i], linewidth = 1)
    }
    
    gg <- gg + 
        facet_grid(.data$label ~ ., scales = 'free_y') + 
        xlab('Time') +
	    ylab('')
    
	return(gg)
}