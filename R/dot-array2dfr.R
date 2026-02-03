#' @title MCMC array to data frame
#' @param object \code{bdm} class object
#' @param value_to column header for array value
.array2dfr <- function(object, value_to = "value") {
    
    dm <- dimnames(object)
    
    if (any(grepl("iter",  names(dm)))) dm[[which(grepl("iter",  names(dm)))]] <- 1:dim(object)[1]
    if (any(grepl("chain", names(dm)))) dm[[which(grepl("chain", names(dm)))]] <- 1:dim(object)[2]
    
    dimnames(object) <- dm
    
    # melt to data frame
    object <- array2DF(object, responseName = value_to)
    
    # coerce iterations to integer values
    class(object[,which(grepl("iter", colnames(object)))]) <- "integer"
    
    # return
    return(object)
    
}
