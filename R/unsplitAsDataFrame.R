#' Unsplit a BumpyMatrix 
#'
#' Unsplit a BumpyMatrix into a \linkS4class{DataFrame}, adding back the row and column names as separate columns.
#' This is equivalent to converting the BumpyMatrix into a \dQuote{long} format.
#'
#' @param x A BumpyMatrix object.
#' @param row.names,column.names Logical scalar indicating whether the row or column names of \code{x} should be reported in the output.
#' @param row.field,column.field String indicating the field in the output DataFrame to store the row or column names.
#' @param value.field String specifying the field in the output DataFrame to store BumpyAtomicMatrix values.
#'
#' @return A \linkS4class{DataFrame} object containing the data in \code{x}.
#' This has additional \code{row} and \code{column} columns containing the row/column names for each DataFrame row.
#'
#' If \code{x} is a \linkS4class{BumpyAtomicMatrix}, the output DataFrame contains a \code{value} column that holds \code{unlist(x)}.
#' Otherwise, if \code{x} is a \linkS4class{BumpyDataFrameMatrix}, the DataFrame contains the columns in \code{unlist(x)}.
#' 
#' @details
#' Denote the output of this function as \code{y}.
#' Given a BumpyAtomicMatrix \code{x}, we would expect to be able to recover \code{x} by calling \code{\link{splitAsBumpyMatrix}(y$value, y$row, y$column)}.
#'
#' The \code{row.field}, \code{column.field} and \code{value.field} arguments can be used to alter the column names of the output DataFrame.
#' This can be helpful to avoid, e.g., conflicts with columns of the same name in a BumpyDataFrameMatrix \code{x}.
#'
#' If no row/column names are present in \code{x} (or \code{row.names} or \code{column.names} is \code{FALSE}),
#' the \code{row} and \code{column} columns instead hold integer indices specifying the matrix row/column of each DataFrame row.
#'
#' @examples
#' mat <- splitAsBumpyMatrix(runif(1000), 
#'    row=sample(LETTERS, 1000, replace=TRUE),
#'    column=sample(10, 1000, replace=TRUE)
#' )
#' 
#' unsplitAsDataFrame(mat)
#'
#' @seealso
#' \code{\link{splitAsBumpyMatrix}}, to do the split in the first place.
#'
#' @author Aaron Lun
#' @export
#' @importFrom S4Vectors DataFrame 
unsplitAsDataFrame <- function(x, row.names=TRUE, column.names=TRUE, row.field="row", column.field="column", value.field="value") {
    basic <- undim(x)
    current <- which(x@proxy!=0, arr.ind=TRUE)

    rows <- rep(current[,1], lengths(basic))
    if (row.names && !is.null(rownames(x))) {
        rows <- rownames(x)[rows]
    }

    cols <- rep(current[,2], lengths(basic))
    if (column.names && !is.null(colnames(x))) {
        cols <- colnames(x)[cols]
    }

    coords <- DataFrame(row=rows, column=cols)    
    colnames(coords) <- c(row.field, column.field)

    contents <- unlist(basic, use.names=FALSE)
    if (is(contents, "DataFrame")) {
        # Using contents[,0] to preserve row names and any metadata.
        output <- cbind(contents[,0], coords, contents)
    } else {
        output <- coords
        output[[value.field]] <- contents
    }

    output
}
