#' Split to a BumpyMatrix
#'
#' Split a vector or \linkS4class{Vector} into a BumpyMatrix based on row/column factors.
#' This facilitates the construction of a BumpyMatrix from vector-like objects.
#'
#' @param x A vector or \linkS4class{Vector} object, most typically a \linkS4class{DFrame}.
#' @param row An object coercible into a factor, of length equal to \code{x}.
#' This defines the row index for each element of \code{x}.
#' @param column An object coercible into a factor, of length equal to \code{x}.
#' This defines the column index for each element of \code{x}.
#' @param sparse Logical scalar indicating whether a sparse representation should be used.
#'
#' @return A \linkS4class{BumpyMatrix} of the appropriate type,
#' with number of rows and columns equal to the number of levels in \code{row} and \code{column} respectively.
#' Each entry of the matrix contains all elements of \code{x} with the corresponding indices in \code{row} and \code{column}.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{BumpyMatrix}}, if a \linkS4class{CompressedList} has already been constructed.
#'
#' @examples
#' mat <- splitToBumpyMatrix(runif(1000), 
#'    row=sample(LETTERS, 1000, replace=TRUE),
#'    column=sample(10, 1000, replace=TRUE)
#' )
#' mat
#' mat[,1]
#' mat[1,]
#'
#' # Or with a sparse representation.
#' mat <- splitToBumpyMatrix(runif(10), 
#'    row=sample(LETTERS, 10, replace=TRUE),
#'    column=sample(10, 10, replace=TRUE)
#' )
#' mat
#' mat[,1]
#' mat[1,]
#'
#' @export
#' @importFrom Matrix sparseMatrix
splitToBumpyMatrix <- function(x, row, column, sparse=FALSE) {
    row <- as.factor(row)
    column <- as.factor(column)

    # TODO: avoid running into the .Machine$integer.max limit. 
    i <- as.integer(row) + (as.integer(column) - 1L) * nlevels(row)

    if (!sparse) {
        f <- factor(i, seq_len(nlevels(row) * nlevels(column)))
        out <- splitAsList(x, f)
    } else {
        out <- splitAsList(x, i)
    }

    if (!sparse) {
        BumpyMatrix(out, dim=c(nlevels(row), nlevels(column)), 
            dimnames=list(levels(row), levels(column)))
    } else {
        idx <- as.integer(names(out))
        proxy <- sparseMatrix(
            i=((idx - 1L) %% nlevels(row)) + 1L,
            j=as.integer((idx - 1L)/nlevels(row)) + 1L,
            x=seq_along(out),
            dims=c(nlevels(row), nlevels(column)),
            dimnames=list(levels(row), levels(column))
        )
        BumpyMatrix(out, proxy=proxy, reorder=FALSE)
    }
}
