#' The BumpyMatrix class
#'
#' The BumpyMatrix provides a two-dimensional object where each entry is a \linkS4class{Vector} object.
#' This is useful for storing data that has a variable number of observations per sample/feature combination,
#' e.g., for inclusion as another assay in a SummarizedExperiment object.
#'
#' @section Constructor:
#' \code{BumpyMatrix(x, dims, dimnames=list(NULL, NULL), proxy=NULL, reorder=TRUE)} will produce a BumpyMatrix object, given:
#' \itemize{
#' \item \code{x}, a \linkS4class{CompressedList} object containing one or more \linkS4class{DFrame}s or atomic vectors.
#' \item \code{dim}, an integer vector of length 2 specifying the dimensions of the returned object.
#' \item \code{dimnames}, a list of length 2 containing the row and column names.
#' \item \code{proxy}, an integer or numeric matrix-like object specifying the location of each entry of \code{x} in the output matrix.
#' \item \code{reorder}, a logical scalar indicating whether \code{proxy} (if specified) should be reordered.
#' }
#' The type of the returned BumpyMatrix object is determined from the type of \code{x}.
#'
#' If \code{proxy=NULL}, \code{x} should have length equal to the product of \code{dim}.
#' The entries of the returned BumpyMatrix are filled with \code{x} in a column-major manner.
#'
#' If \code{proxy} is specified, it should contain indices in \code{1:length(x)} with all other entries filled with zeros.
#' If \code{reorder=FALSE}, all non-zero values should be in increasing order when encountered in column-major format;
#' otherwise, the indices are resorted to enforce this expectation.
#' Note that \code{dims} and \code{dimnames} are ignored.
#'
#' If \code{x} is missing, a \linkS4class{BumpyIntegerMatrix} is returned with zero rows and columns.
#' If \code{dim} is also specified, a BumpyIntegerMatrix with the specified number of rows and columns is returned,
#' where each entry is an empty integer vector.
#' 
#' @section Basic matrix methods:
#' In the following code snippets, \code{x} is an instance of a BumpyMatrix subclass.
#'
#' \code{dim(x)} will yield a length-2 integer vector containing the number of rows and columns in \code{x}.
#' \code{length(x)} will yield the product of the number of columns and rows.
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
#' \code{lengths(x)} will return a numeric matrix-like object with the same dimensions and dimnames as \code{x},
#' where each entry contains the length of the corresponding entry in \code{x}.
#' The output class can be anything used in the \code{proxy} of the constructor, e.g., a sparse matrix from the \pkg{Matrix} package.
#'
#' @examples
#' # Mocking up a BumpyNumericList:
#' library(IRanges)
#' x <- NumericList(split(runif(1000), factor(sample(50, 1000, replace=TRUE), 1:50)))  
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
#' [,BumpyMatrix,ANY-method
#' [,BumpyMatrix,BumpyMatrix,ANY,ANY-method
#' [,BumpyMatrix,BumpyMatrix-method
#' [<-,BumpyMatrix,ANY,ANY,BumpyMatrix-method
#' dim,BumpyMatrix-method
#' length,BumpyMatrix-method
#' dimnames,BumpyMatrix-method
#' dimnames<-,BumpyMatrix,ANY-method
#' rbind,BumpyMatrix-method
#' cbind,BumpyMatrix-method
#' t,BumpyMatrix-method
#' undim
#' undim,BumpyMatrix-method
#' unlist,BumpyMatrix-method
#' lengths,BumpyMatrix-method
NULL

#' @export
#' @importFrom Matrix sparseMatrix
BumpyMatrix <- function(x, dim, dimnames=list(NULL, NULL), proxy=NULL, reorder=TRUE) {
    if (missing(x)) {
        x <- IntegerList()
        if (is.null(proxy)) {
            if (missing(dim)) {
                dim <- integer(2)
            }
            proxy <- sparseMatrix(i=integer(0), j=integer(0), x=numeric(0), dims=dim)
        }
    }

    if (is(unlist(x), "DataFrame")) {
        # To handle more DataFrame subclasses without writing a new class.
        y <- "DataFrame"
    } else {
        y <- x@elementType
        substring(y, 1L, 1L) <- toupper(substring(y, 1L, 1L))
    }

    if (is.null(proxy)) {
        proxy <- matrix(seq_len(dim[1]*dim[2]), dim[1], dim[2], dimnames=dimnames)
        reorder <- FALSE
    }

    if (reorder) {
        out <- .reorder_indices_raw(proxy, x)
        proxy <- out[[1]]
        x <- out[[2]]
    }

    new(paste0("Bumpy", y, "Matrix"), data=x, proxy=proxy)
}

setValidity("BumpyMatrix", function(object) {
    msg <- character(0)

    if (length(dim(object@proxy))!=2L) {
        msg <- c(msg, "'proxy' should be a 2-dimensional object")
    }
    if (!is.numeric(object@proxy[0])) {
        msg <- c(msg, "'proxy' should contain integer indices")
    } else {
        nzero <- which(object@proxy!=0)
        if (!identical(as.integer(object@proxy[nzero]), seq_along(nzero))) {
            msg <- c(msg, "'proxy' should contain consecutive indices")
        }
        if (length(nzero)!=length(undim(object))) {
            msg <- c(msg, "'data' should have length equal to the number of non-zero elements in 'proxy'")
        }
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("show", "BumpyMatrix", function(object) {
    cat(sprintf("%i x %i %s\n", nrow(object), ncol(object), class(object)[1])) 

    if (!is.null(rownames(object))) {
        lbls <- paste(S4Vectors:::selectSome(rownames(object)), collapse = " ")
    } else {
        lbls <- "NULL"
    }
    cat("rownames:", lbls, "\n")

    if (!is.null(colnames(object))) {
        lbls <- paste(S4Vectors:::selectSome(colnames(object)), collapse = " ")
    } else {
        lbls <- "NULL"
    }
    cat("colnames:", lbls, "\n")
})

#' @export
setMethod("dim", "BumpyMatrix", function(x) dim(x@proxy))

#' @export
setMethod("length", "BumpyMatrix", function(x) length(x@proxy))

#' @export
setMethod("dimnames", "BumpyMatrix", function(x) dimnames(x@proxy))

#' @export
setReplaceMethod("dimnames", "BumpyMatrix", function(x, value) {
    dimnames(x@proxy) <- value
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

    # Goddamn S4 subsetting doesn't dispatch on substituted missing arguments.
    if (!missing(i) && !missing(j)) {
        leftovers <- x@proxy[i,j,drop=FALSE]
    } else if (!missing(i)) {
        leftovers <- x@proxy[i,,drop=FALSE]
    } else {
        leftovers <- x@proxy[,j,drop=FALSE]
    }

    nzero <- which(leftovers!=0)
    idx <- leftovers[nzero]
    x@data <- x@data[idx]
    leftovers[nzero] <- seq_along(nzero)
    x@proxy <- leftovers

    if (drop && any(dim(x)==1L)) {
        indices <- drop(x@proxy)
        output <- .expand_List(x@data, indices)
        names(output) <- names(indices)
        output
    } else {
        x
    }
})

.commat_by_commat <- function(x, i, ...) {
    # DO NOT DELETE! Needed in DFrameMatrix, do not delete.
    if (!identical(dim(x), dim(i))) {
        stop("BumpyMatrix objects 'x' and 'i' should have the same dimensions")
    }
    values <- .reconcile_matrices(list(x, i))
    x <- values[[1]]
    i <- values[[2]]
    x@data <- undim(x)[undim(i)]
    x
}

#' @export
setMethod("[", c("BumpyMatrix", "BumpyMatrix"), .commat_by_commat)

#' @export
setReplaceMethod("[", c("BumpyMatrix", "ANY", "ANY", "BumpyMatrix"), function(x, i, j, ..., value) {
    bumped <- .increment_indices(value@proxy, length(undim(x)))

    # Goddamn S4 subsetting doesn't dispatch on substituted missing arguments.
    if (!missing(i) && !missing(j)) {
        x@proxy[i,j] <- bumped
    } else if (!missing(i)) {
        x@proxy[i,] <- bumped
    } else if (!missing(j)) {
        x@proxy[,j] <- bumped
    } else {
        x@proxy[] <- bumped
    }

    x@data <- c(x@data, value@data)
    .reorder_indices(x)
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
        reorder[[i]] <- .increment_indices(current@proxy, offset)
        offset <- offset + length(combined[[i]])
    }

    reorder <- do.call(rbind, reorder)
    combined <- do.call(c, combined)

    # This might be a new class, depending on the type; can't just use arg[[1]].
    BumpyMatrix(combined, proxy=reorder)
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
        reorder[[i]] <- .increment_indices(current@proxy, offset)
        offset <- offset + length(combined[[i]])
    }

    reorder <- do.call(cbind, reorder)
    combined <- do.call(c, combined)

    # This might be a new class, depending on the type; can't just use arg[[1]].
    BumpyMatrix(combined, proxy=reorder)
})

#' @export
#' @importFrom Matrix t
setMethod("t", "BumpyMatrix", function(x) {
    x@proxy <- t(x@proxy) 
    .reorder_indices(x)
})

#' @export
setMethod("lengths", "BumpyMatrix", function(x) {
    out <- x@proxy
    n <- lengths(undim(x))

    # Use arr.ind=TRUE to avoid problems with 
    # []<- for real indices in Matrix classes.
    out[which(out!=0, arr.ind=TRUE)] <- n

    out
})
