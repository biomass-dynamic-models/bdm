#' @title Return model parameters (including input data)
#' 
#' @param object a \code{bdm} class object 
#' @param ... additional arguments to generic function
#' 
#' @export
getparams <- function(object, ...) UseMethod("getparams")
#'
#' @include getcpp.R
#' @rdname getparams
#' @export
getparams.bdm <- function(object, ...) {
    
    cpp <- getcpp(object)
    
    params <- grep("names__ = std::vector<std::string>{", fixed = TRUE, value = TRUE, x = strsplit(cpp, "\n")[[1]])
    params <- sapply(strsplit(params, "\""), FUN = function(x) x[[2]])
    
    return(params)
}
