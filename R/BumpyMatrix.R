#' The BumpyMatrix class
#'
#' The BumpyMatrix provides a two-dimensional object where each entry is a \linkS4class{Vector} object.
#' This is useful for storing data that has a variable number of observations per sample/feature combination,
#' e.g., for inclusion as another assay in a SummarizedExperiment object.
#'
#' @section Constructor:
#' \code{BumpyMatrix(x, dims, dimnames=list(NULL, NULL))} will produce a DataFrameMatrix object, given:
#' \itemize{
#' \item \code{x}, a \linkS4class{CompressedList} object containing one or more \linkS4class{DFrame}s or atomic vectors.
#' \item \code{dim}, an integer vector of length 2 specifying the dimensions of the returned object.
#' \item \code{dimnames}, a list of length 2 containing the row and column names.
#' }
#' \code{x} should have length equal to the product of \code{dim}.
#' The entries of the returned DataFrameMatrix are filled by \code{x} in a column-major manner.
#'
#' @section Basic matrix methods:
#' In the following code snippets, \code{x} is an instance of a BumpyMatrix subclass.
#'
#' \code{dim(x)} will yield a length-2 integer vector containing the number of rows and columns in \code{x}.
#'
#' \code{dimnames(x)} will yield a list of two character vectors with the row and column names of \code{x}.
#' Either or both elements of the list may be \code{NULL} if no names are present.
#'
#' \code{x[i, j, ..., drop=TRUE]} will yield the specified submatrix of the same type as \code{x},
#' given integer, character or logical subsetting vectors in \code{i} and \code{j}.
#' If the resulting submatrix has any dimension of length 1 and \code{drop=TRUE},
#' a \linkS4class{CompressedList} of the appropriate type is instead returned.
#'
#' \code{x[i,j] <- value} will replace the specified entries in \code{x} with the values in another BumpyMatrix \code{value}.
#' It is expected that \code{value} is of the same subclass as \code{x}.
#'
#' \code{t(x)} will transpose the BumpyMatrix, returning an object of the same type.
#'
#' \code{rbind(..., deparse.level=1)} and \code{cbind(..., deparse.level=1)} will combine all BumpyMatrix objects in \code{...},
#' yielding a single BumpyMatrix object containing all the rows and columns, respectively.
#' All objects should have the same number of columns (for \code{rbind}) or rows (for \code{cbind}).
#'
#' @section Subsetting by another BumpyMatrix:
#' Given a BumpyMatrix \code{x} and an appropriate BumpyMatrix \code{i},
#' \code{x[i]} will return another BumpyMatrix where each entry of \code{x} is subsetted by the corresponding entry of \code{i}.
#' This usually requires \code{i} to be a BumpyIntegerMatrix or a BumpyLogicalMatrix,
#' though it is also possible to use a BumpyCharacterMatrix if each entry of \code{x} is named.
#'
#' @section Special compressed methods:
#' \code{undim(x)} will return the underlying \linkS4class{CompressedList} object.
#'
#' \code{unlist(x, ...)} will return the underlying \linkS4class{Vector} used to create the \linkS4class{CompressedList} object.
#' This is the same as \code{unlist(undim(x), ...)}.
#'
#' @examples
#' # Mocking up a BumpyNumericList:
#' library(IRanges)
#' x <- NumericList(split(runif(1000), factor(sample(20, 50, replace=TRUE), 1:50)))  
#' length(x)
#'
#' # Creating a BumpyNumericMatrix:
#' mat <- BumpyMatrix(x, c(10, 5))
#' mat
#'
#' # Standard subsetting works correctly:
#' mat[1:10,1:2]
#' mat[,1]
#' mat[1,]
#' 
#' # Subsetting by another BumpyMatrix.
#' is.big <- x > 0.9
#' i <- BumpyMatrix(is.big, dim(mat))
#' out <- mat[i]
#' out # same dimensions as mat...
#' out[,1] # but the entries are subsetted.
#' out[1,]
#' 
#' # Subset replacement works correctly:
#' mat[,2]
#' alt <- mat
#' alt[,2] <- mat[,1,drop=FALSE]
#' alt[,2]
#' 
#' # Combining works correctly:
#' rbind(mat, mat)
#' cbind(mat, mat)
#'
#' # Transposition works correctly:
#' mat[1,2]
#' tmat <- t(mat)
#' tmat
#' tmat[1,2]
#'
#' # Get the underlying objects:
#' undim(mat)
#' summary(unlist(mat))
#' 
#' @author Aaron Lun
#'
#' @name BumpyMatrix 
#' @docType class
#' @aliases
#' BumpyMatrix-class
#' show,BumpyMatrix-method
#' [,BumpyMatrix,ANY,ANY,ANY-method
#' [<-.BumpyMatrix,ANY,ANY,BumpyMatrix-method
#' rbind,BumpyMatrix-method
#' cbind,BumpyMatrix-method
#' t,BumpyMatrix-method
#' undim
#' undim,BumpyMatrix-method
#' unlist,BumpyMatrix-method
NULL

#' @export
BumpyMatrix <- function(x, dim, dimnames=list(NULL, NULL)) {
    y <- x@elementType
    substring(y, 1L, 1L) <- toupper(substring(y, 1L, 1L))
    new(paste0("Bumpy", y, "Matrix"), data=x, dim=as.integer(dim), dimnames=dimnames)
}

#' @export
setMethod("initialize", "BumpyMatrix", function(.Object, ..., dimnames=list(NULL, NULL)) {
    callNextMethod()
})

setValidity("BumpyMatrix", function(object) {
    msg <- character(0)

    if (length(undim(object))!=nrow(object) * ncol(object)) {
        msg <- c(msg, "product of 'dim(object)' should be equal to the length of 'undim(object)'")
    }

    if (!is.null(rownames(object)) && length(rownames(object))!=nrow(object)) {
        msg <- c(msg, "'rownames(object)' should be NULL or have length equal to 'nrow(object)'")
    }
    if (!is.null(colnames(object)) && length(colnames(object))!=ncol(object)) {
        msg <- c(msg, "'colnames(object)' should be NULL or have length equal to 'ncol(object)'")
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

setMethod("show", "BumpyMatrix", function(object) {
    # TODO: think about adding some more detail to preview here.
    cat(sprintf("%i x %i %s\n", nrow(object), ncol(object), class(object)[1])) 
})

#' @export
setMethod("dimnames", "BumpyMatrix", function(x) x@dimnames)

#' @export
setReplaceMethod("dimnames", "BumpyMatrix", function(x, value) {
    if (is.null(value)) {
        value <- list(NULL, NULL)
    }
    x@dimnames <- value
    validObject(x)
    x
})

#' @export
setMethod("unlist", "BumpyMatrix", function(x, recursive = TRUE, use.names = TRUE) {
    unlist(undim(x), recursive=recursive, use.names=use.names)
})

#' @export
setMethod("undim", "BumpyMatrix", function(x) x@data)

#' @export
setMethod("[", c("BumpyMatrix", "ANY", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE) {
    if (missing(i) && missing(j)) {
        return(x)
    }

    # TODO: avoid instantiating the entire Matrix.
    keep <- matrix(seq_len(nrow(x)*ncol(x)), nrow(x), ncol(x), dimnames=dimnames(x))
    if (!missing(i)) {
        keep <- keep[i,,drop=FALSE] 
    }
    if (!missing(j)) {
        keep <- keep[,j,drop=FALSE]
    }

    x@data <- undim(x)[as.integer(keep)]
    x@dim <- dim(keep)
    x@dimnames <- dimnames(keep)

    if (drop && any(dim(x)==1L)) {
        stuff <- undim(x)
        if (nrow(x)==1L && ncol(x)==1L) {
            n <- NULL
        } else if (nrow(x)==1L) {
            n <- colnames(keep)
        } else {
            n <- rownames(keep)
        }
        names(stuff) <- n
        return(stuff)
    }

    x
})

.commat_by_commat <- function(x, i, ...) {
    # DO NOT DELETE! Needed in DFrameMatrix, do not delete.
    if (!identical(dim(x), dim(i))) {
        stop("BumpyMatrix objects 'x' and 'i' should have the same dimensions")
    }
    x@data <- undim(x)[undim(i)]
    x
}

#' @export
setMethod("[", c("BumpyMatrix", "BumpyMatrix"), .commat_by_commat)

#' @export
setReplaceMethod("[", c("BumpyMatrix", "ANY", "ANY", "BumpyMatrix"), function(x, i, j, ..., value) {
    # TODO: avoid instantiating the entire Matrix.
    keep <- matrix(seq_len(nrow(x)*ncol(x)), nrow(x), ncol(x), dimnames=dimnames(x))
    if (!missing(i)) {
        keep <- keep[i,,drop=FALSE] 
    }
    if (!missing(j)) {
        keep <- keep[,j,drop=FALSE]
    }

    x@data[as.integer(keep)] <- undim(value)

    x
})

#' @export
setMethod("rbind", "BumpyMatrix", function(..., deparse.level=1) {
    args <- list(...)
    if (length(unique(vapply(args, ncol, 0L)))!=1L) {
        stop("all objects in 'cbind(<BumpyMatrix>)' should have the same 'ncol'")
    }

    combined <- reorder <- vector("list", length(args))
    offset <- 0L

    for (i in seq_along(combined)) {
        current <- args[[i]]
        combined[[i]] <- undim(current)
        reorder[[i]] <- matrix(offset + seq_along(combined[[i]]), 
            nrow(current), ncol(current), dimnames=dimnames(current))
        offset <- offset + length(combined[[i]])
    }

    reorder <- do.call(rbind, reorder)
    combined <- do.call(c, combined)
    combined <- combined[as.vector(reorder)]

    if (is.null(dn <- dimnames(reorder))) {
        dn <- list(NULL, NULL)
    }
    # This might be a new class, depending on the type; can't just use arg[[1]].
    BumpyMatrix(combined, dim=dim(reorder), dimnames=dn)
})

#' @export
setMethod("cbind", "BumpyMatrix", function(..., deparse.level=1) {
    args <- list(...)
    if (length(unique(vapply(args, nrow, 0L)))!=1L) {
        stop("all objects in 'rbind(<BumpyMatrix>)' should have the same 'nrow'")
    }

    combined <- reorder <- vector("list", length(args))
    offset <- 0L

    for (i in seq_along(combined)) {
        current <- args[[i]]
        combined[[i]] <- undim(current)
        reorder[[i]] <- matrix(offset + seq_along(combined[[i]]), 
            nrow(current), ncol(current), dimnames=dimnames(current))
        offset <- offset + length(combined[[i]])
    }

    reorder <- do.call(cbind, reorder)
    combined <- do.call(c, combined)
    combined <- combined[as.vector(reorder)]

    if (is.null(dn <- dimnames(reorder))) {
        dn <- list(NULL, NULL)
    }
    # This might be a new class, depending on the type; can't just use arg[[1]].
    BumpyMatrix(combined, dim=dim(reorder), dimnames=dn)
})

#' @export
setMethod("t", "BumpyMatrix", function(x) {
    o <- matrix(seq_len(nrow(x)*ncol(x)), ncol(x), nrow(x), byrow=TRUE)
    x@data <- undim(x)[as.vector(o)]
    x@dim <- rev(x@dim)
    x@dimnames <- rev(x@dimnames)
    x
})
