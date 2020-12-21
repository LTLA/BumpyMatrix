#' @export 
setGeneric("undim", function(x) standardGeneric("undim"))

#' @export
setGeneric("redim", function(flesh, skeleton = attr(flesh, "skeleton")) standardGeneric("redim"))
