#' The BumpyDFrameMatrix class
#'
#' The BumpyDFrameMatrix provides a two-dimensional object where each entry is a \linkS4class{DataFrame}.
#' This is useful for storing data that has a variable number of observations per sample/feature combination,
#' e.g., for inclusion as another assay in a SummarizedExperiment object.
#'
#' @details
#' In the following code snippets, \code{x} is a BumpyDFrameMatrix.
#'
#' \code{fields(x)} will return a character vector with the names of the available fields.
#' This can be modified with \code{fields(x) <- value}.
#'
#' \code{x[i, j, k, ..., .dropk=drop, drop=TRUE]} will subset the BumpyDFrameMatrix:
#' \itemize{
#' \item If \code{k} is not specified, this will either produce another BumpyDFrameMatrix corresponding to the specified submatrix,
#' or a \linkS4class{CompressedSplitDFrameList} containing the entries of interest if \code{drop=TRUE}.
#' \item If \code{k} is specified, it should contain the names or indices of the columns of the underlying DataFrame to retain.
#' For multiple fields or with \code{.dropk=FALSE}, a new BumpyDFrameMatrix is returned with the specified columns in the DataFrame.
#' \item If \code{k} only specifies a single column and \code{.dropk=TRUE},
#' a BumpyMatrix (or \linkS4class{CompressedList}, if \code{drop=TRUE}) corresponding to the type of the field is returned.
#' }
#'
#' \code{x[i, j, k, ...] <- value} will modify \code{x} by replacing the specified values with those in the BumpyMatrix \code{value} of the same dimensions.
#' If \code{k} is not specified, \code{value} should be a BumpyDFrameMatrix with the same fields as \code{x}.
#' If \code{k} is specified, \code{value} should be a BumpyDFrameMatrix with the specified fields.
#' If \code{k} contains a single field, \code{value} can also be a BumpyAtomicMatrix containing the values to use in that field.
#'
#' All methods described for the \linkS4class{BumpyMatrix} parent class are available.
#' 
#' @author Aaron Lun
#'
#' @examples
#' library(S4Vectors)
#' df <- DataFrame(x=runif(100), y=runif(100))
#' f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
#' out <- split(df, f)
#'
#' # Making our BumpyDFrameMatrix.
#' mat <- BumpyMatrix(out, c(5, 4))
#' mat[,1]
#' mat[1,]
#'
#' # Subsetting capabilities.
#' xmat <- mat[,,"x"]
#' ymat <- mat[,,"y"]
#' filtered <- mat[xmat > 0.5 & ymat > 0.5]
#' filtered[,1]
#'
#' # Subset replacement works as expected.
#' mat2 <- mat
#' mat2[,,"x"] <- mat2[,,"x"] * 2
#' mat2[,1]
#' 
#' @name BumpyDFrameMatrix
#' @docType class
#' @aliases
#' BumpyDFrameMatrix-class
#' show,BumpyDFrameMatrix-method
#' fields
#' fields,BumpyDFrameMatrix-method
#' fields<-
#' fields<-,BumpyDFrameMatrix-method
#' [,BumpyDFrameMatrix,ANY-method
#' [,BumpyDFrameMatrix,ANY,ANY,ANY-method
#' [,BumpyDFrameMatrix,BumpyMatrix-method
#' [,BumpyDFrameMatrix,BumpyMatrix,ANY,ANY-method
#' [<-,BumpyDFrameMatrix,ANY,ANY,BumpyMatrix-method
NULL

#' @export
setMethod("show", "BumpyDFrameMatrix", function(object) {
    callNextMethod()
    coolcat("fields(%i): %s", vals=colnames(unlist(object)))    
})

#' @export
setMethod("[", "BumpyDFrameMatrix", function(x, i, j, k, ..., .dropk=drop, drop=TRUE) {
    output <- callNextMethod(x, i=i, j=j, ..., drop=drop)
   
    if (!missing(k)) {
        if (is(output, "CompressedList")) {
            output <- output[,k,drop=.dropk]
        } else {
            sub <- undim(output)[,k,drop=.dropk]
            if (is(sub, "CompressedSplitDFrameList")) {
                output@data <- sub
            } else {
                output <- BumpyMatrix(sub, proxy=output@proxy, reorder=FALSE)
            }
        }
    } 

    output
})

#' @export
setMethod("fields", "BumpyDFrameMatrix", function(x) colnames(unlist(x)))

#' @export
setReplaceMethod("fields", "BumpyDFrameMatrix", function(x, value) {
    # Get IRanges to add this capability directly?
    colnames(x@data@unlistData) <- value
    x
})

#' @export
setMethod("[", c("BumpyDFrameMatrix", "BumpyMatrix"), .commat_by_commat) # redefinition to disambiguate dispatch.

#' @export
setReplaceMethod("[", c("BumpyDFrameMatrix", "ANY", "ANY", "BumpyMatrix"), function(x, i, j, k, ..., value) {
    if (missing(k)) {
        callNextMethod(x, i=i, j=j, ..., value=value)
    } else {
        if (missing(i) && missing(j)) {
            out <- .reconcile_matrices(list(x, value))
            x <- out[[1]]
            value <- out[[2]]
            x@data[,k] <- undim(value)

        } else {
            if (!missing(i) && !missing(j)) {
                sub <- x[i,j,drop=FALSE]
            } else if (!missing(i)) {
                sub <- x[i,,drop=FALSE]
            } else {
                sub <- x[,j,drop=FALSE]
            }

            out <- .reconcile_matrices(list(sub, value))
            sub <- out[[1]]
            value <- out[[2]]
            sub@data[,k] <- undim(value)

            if (!missing(i) && !missing(j)) {
                x[i,j] <- sub
            } else if (!missing(i)) {
                x[i,] <- sub
            } else {
                x[,j] <- sub
            }
        }

        x 
    }
})
