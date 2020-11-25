#' The BumpyAtomicMatrix subclass
#'
#' A subclass of the \linkS4class{BumpyMatrix} where each entry is an atomic vector.
#' One subclass is provided for each of the most common types.
#'
#' @details
#' In the following code snippets, \code{x} is a BumpyDFrameMatrix.
#'
#' Binary and unary operations are implemented by specializing \link{Ops}, \link{Math} and related group generics,
#' and will usually return a new BumpyAtomicMatrix of the appropriate type.
#' The exception is for certain \link{Summary} methods that return an ordinary matrix,
#' where the operation is guaranteed to return a scalar for each entry of the input BumpyAtomicMatrix (e.g., \code{\link{min}}).
#' 
#' Common mathematical operations are implemented that apply to each entry of the BumpyAtomicMatrix:
#' \itemize{
#' \item \code{mean}, \code{sd}, \code{median}, \code{mad} and \code{var} take a single BumpyAtomicMatrix
#' and return an ordinary matrix of the same dimensions containing the computed statistic for each entry of the input.
#' This is possible as all operations are guaranteed to produce a scalar.
#' \item \code{which.max} and \code{which.min} take a single BumpyAtomicMatrix
#' and return a BumpyIntegerMatrix of the same dimensions containing zero or 1 integer indices for the min/max value.
#' \item \code{pmin}, \code{pmax}, \code{pmin.int} and \code{pmax.int} take multiple BumpyAtomicMatrix objects of the same dimensions,
#' and return a BumpyAtomicMatrix containing the maximum/minimum value across all objects for the same entry. 
#' \item \code{cor}, \code{cov} and (optionally) \code{var} take two BumpyAtomicMatrix objects of the same dimensions,
#' and return an ordinary matrix containing the computed statistic for the corresponding entries of the inputs. 
#' This is possible as all operations are guaranteed to produce a scalar.
#' \item \code{quantile} and \code{IQR} take a single BumpyAtomicMatrix as input 
#' and return a BumpyNumericMatrix of the same dimensions, containing the vector of statistics.
#' }
#' 
#' Additionally, common operations are implemented that apply to each entry of the BumpyCharacterMatrix
#' and return a BumpyAtomicMatrix of the same dimensions and an appropriate type.
#' This includes \code{tolower}, \code{toupper}, \code{substr}, \code{substring}, \code{sub}, \code{gsub}, \code{grepl}, \code{grep},
#' \code{nchar}, \code{chartr}, \code{startsWith} and \code{endsWith}.
#' We also implement \code{unstrsplit}, which returns an ordinary matrix of the same dimensions containing the unsplit strings.
#'
#' All methods implemented for the \linkS4class{BumpyMatrix} parent class are available here.
#'
#' @examples
#' # Mocking up a BumpyNumericList:
#' library(IRanges)
#' x <- NumericList(split(runif(1000), 
#'     factor(sample(50, 1000, replace=TRUE), 1:50)))  
#'
#' # Creating a BumpyNumericMatrix:
#' mat <- BumpyMatrix(x, c(10, 5))
#' mat[,1]
#'
#' # Arithmetic operations:
#' (mat * 2)[,1]
#' (mat + mat * 5)[,1]
#'
#' # Logical operations:
#' (mat < 0.5)[,1]
#' (mat > 0.5 & mat < 1)[,1]
#' (mat == mat)[,1]
#'
#' # More statistics:
#' max(mat)
#' min(mat)
#' mean(mat)
#' sd(mat)
#' median(mat)
#'
#' # Handling character vectors:
#' x <- CharacterList(split(sample(LETTERS, 100, replace=TRUE), 
#'     factor(sample(20, 100, replace=TRUE), 1:20)))  
#' cmat <- BumpyMatrix(x, c(5, 4))
#' cmat[,1]
#'
#' tolower(cmat[,1])
#' grepl("A", cmat)[,1]
#' sub("A", "whee", cmat)[,1]
#' 
#' @aliases
#' BumpyAtomicMatrix-class
#' BumpyIntegerMatrix-class
#' BumpyCharacterMatrix-class
#' BumpyNumericMatrix-class
#' BumpyLogicalMatrix-class
#' Ops,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' Ops,atomic,BumpyAtomicMatrix-method
#' Ops,BumpyAtomicMatrix,atomic-method
#' Math,BumpyAtomicMatrix-method
#' Math2,BumpyAtomicMatrix-method
#' Summary,BumpyAtomicMatrix-method
#'
#' mean,BumpyAtomicMatrix-method
#' sd,BumpyAtomicMatrix-method
#' median,BumpyAtomicMatrix-method
#' mad,BumpyAtomicMatrix-method
#' var,BumpyAtomicMatrix,missing-method
#' which.min,BumpyAtomicMatrix-method
#' which.max,BumpyAtomicMatrix-method
#' pmin,BumpyAtomicMatrix-method
#' pmax,BumpyAtomicMatrix-method
#' pmin.int,BumpyAtomicMatrix-method
#' pmax.int,BumpyAtomicMatrix-method
#' var,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' cov,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' cor,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#'
#' unstrsplit,BumpyCharacterMatrix-method
#' nchar,BumpyCharacterMatrix-method
#' substring,BumpyCharacterMatrix-method
#' substr,BumpyCharacterMatrix-method
#' chartr,ANY,ANY,BumpyCharacterMatrix-method
#' toupper,BumpyCharacterMatrix-method
#' tolower,BumpyCharacterMatrix-method
#' sub,ANY,ANY,BumpyCharacterMatrix-method
#' gsub,ANY,ANY,BumpyCharacterMatrix-method
#' grep,ANY,BumpyCharacterMatrix-method
#' grepl,ANY,BumpyCharacterMatrix-method
#' startsWith,BumpyCharacterMatrix-method
#' endsWith,BumpyCharacterMatrix-method
#'
#' @name BumpyAtomicMatrix
NULL

#' @export
setMethod("Ops", c("BumpyAtomicMatrix", "BumpyAtomicMatrix"), function(e1, e2) {
    if (!identical(dim(e1), dim(e2))) {
        stop("'e1' and 'e2' must have the same dimensions")
    }
    out <- callGeneric(undim(e1), undim(e2))
    BumpyMatrix(out, dim=dim(e1), dimnames=dimnames(e1))
})

#' @export
setMethod("Ops", c("atomic", "BumpyAtomicMatrix"), function(e1, e2) {
    out <- callGeneric(e1, undim(e2))
    BumpyMatrix(out, dim=dim(e2), dimnames=dimnames(e2))
})

#' @export
setMethod("Ops", c("BumpyAtomicMatrix", "atomic"), function(e1, e2) {
    out <- callGeneric(undim(e1), e2)
    BumpyMatrix(out, dim=dim(e1), dimnames=dimnames(e1))
})

#' @export
setMethod("Math", "BumpyAtomicMatrix", function(x) {
    out <- callGeneric(undim(x))
    BumpyMatrix(out, dim=dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("Math2", "BumpyAtomicMatrix", function(x) {
    out <- callGeneric(undim(x))
    BumpyMatrix(out, dim=dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("Summary", "BumpyAtomicMatrix", function(x) {
    out <- callGeneric(undim(x))
    if (is(out, "CompressedList")) {
        BumpyMatrix(out, dim=dim(x), dimnames=dimnames(x))
    } else {
        matrix(out, nrow(x), ncol(x), dimnames=dimnames(x))
    }
})

#######################################################################
## The following section is derived from IRanges' AtomicList-utils.R ##
#######################################################################

.apply_matrix_out <- function(x, FUN, ..., FUN.VALUE) {
    collected <- vapply(undim(x), FUN, ..., FUN.VALUE)
    matrix(collected, nrow(x), ncol(x), dimnames=dimnames(x))
}

#' @export
setMethods("mean", "BumpyAtomicMatrix", function(x, ...) .apply_matrix_out(x, mean, ..., FUN.VALUE=0))

#' @export
setMethods("sd", "BumpyAtomicMatrix", function(x, na.rm=TRUE) .apply_matrix_out(x, sd, na.rm=na.rm, FUN.VALUE=0))

#' @export
setMethods("median", "BumpyAtomicMatrix", function(x, na.rm=TRUE) .apply_matrix_out(x, median, na.rm=na.rm, FUN.VALUE=0))

#' @export
setMethod("mad", "BumpyAtomicMatrix", function(x, center=median(x), constant=1.4826, na.rm=FALSE, low=FALSE, high=FALSE) {
    if (!missing(center)) {
        stop("'center' argument is not currently supported")
    }
    .apply_matrix_out(x, constant=constant, na.rm=na.rm, low=low, high=high, FUN.VALUE=0)
})

#' @export
setMethod("var", c("BumpyAtomicMatrix", "missing"), function(x, y=NULL, na.rm=FALSE, use) {
    if (missing(use)) {
        use <- ifelse(na.rm, "na.or.complete", "everything")
    }
    .apply_matrix_out(x, var, na.rm=na.rm, use=use, FUN.VALUE=0)
})

.apply_matrix_out_compressed <- function(x, FUN, ..., .CONSTRUCTOR) {
    BumpyMatrix(.CONSTRUCTOR(lapply(undim(x), FUN, ...)), dim(x), dimnames=dimnames(x))
}

#' @export
setMethods("which.min", "BumpyAtomicMatrix", function(x) .apply_matrix_out_compressed(x, which.min, .CONSTRUCTOR=IntegerList))

#' @export
setMethods("which.max", "BumpyAtomicMatrix", function(x) .apply_matrix_out_compressed(x, which.max, .CONSTRUCTOR=IntegerList))

#' @export
setMethod("quantile", "BumpyAtomicMatrix", function(x, ...) .apply_matrix_out_compressed(x, quantile, ..., .CONSTRUCTOR=NumericList))

#' @export
setMethod("IQR", "BumpyAtomicMatrix", function(x, na.rm=FALSE, type=7) 
    .apply_matrix_out_compressed(x, IQR, na.rm=na.rm, type=type, .CONSTRUCTOR=NumericList))

.apply_matrix_out_pm <- function(..., FUN, MoreArgs=list()) {
    args <- list(...)
    if (length(unique(lapply(args, dim)))!=1L) {
        stop("all objects in '", substitute(FUN), "(<BumpyAtomicMatrix>)' should have the same 'dim'")
    }

    output <- lapply(args, undim)
    collected <- do.call(FUN, c(output, MoreArgs))
    BumpyMatrix(collected, dim=dim(args[[1]]), dimnames=dimnames(args[[1]]))
}

#' @export
setMethod("pmax", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out(..., FUN=pmax, MoreArgs = list(na.rm = na.rm)))

#' @export
setMethod("pmax.int", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out(..., FUN=pmax.int, MoreArgs = list(na.rm = na.rm)))

#' @export
setMethod("pmin", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out(..., FUN=pmin, MoreArgs = list(na.rm = na.rm)))

#' @export
setMethod("pmin.int", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out(..., FUN=pmin.int, MoreArgs = list(na.rm = na.rm)))

.apply_matrix_out_dual <- function(x, y, FUN, ...) {
    if (!identical(dim(x), dim(y))) {
        stop("'x' and 'y' must have the same dimensions")
    }
    collected <- FUN(undim(x), undim(y), ...)
    matrix(collected, nrow(x), ncol(x), dimnames=dimnames(x))
}

#' @export
setMethod("var", c("BumpyAtomicMatrix", "BumpyAtomicMatrix"), function(x, y=NULL, na.rm=FALSE, use) {
    if (missing(use)) {
        use <- ifelse(na.rm, "na.or.complete", "everything")
    }
    .apply_matrix_out_dual(x, y, FUN=var, na.rm=na.rm, use=use)
})

#' @export
setMethod("cov", c("BumpyAtomicMatrix", "BumpyAtomicMatrix"),
    function(x, y=NULL, use="everything", method=c("pearson", "kendall", "spearman")) 
{
    .apply_matrix_out_dual(x, y, FUN=cov, method=match.arg(method), use=use)
})

#' @export
setMethod("cor", c("BumpyAtomicMatrix", "BumpyAtomicMatrix"),
    function(x, y=NULL, use="everything", method=c("pearson", "kendall", "spearman"))
{
    .apply_matrix_out_dual(x, y, FUN=cor, method=match.arg(method), use=use)
})

##############################
## Character-specific methods

#' @export
setMethod("unstrsplit", "BumpyCharacterMatrix", function(x, sep="") {
    matrix(unstrsplit(undim(x), sep=sep), nrow(x), ncol(x), dimnames=dimnames(x))              
})

#' @export
setMethod("nchar", "BumpyCharacterMatrix", function(x) {
    BumpyMatrix(nchar(undim(x)), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("substring", "BumpyCharacterMatrix", function(text, first, last = 1000000L) {
    BumpyMatrix(substring(undim(x), first, last), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("substr", "BumpyCharacterMatrix", function(x, start, stop) {
    BumpyMatrix(substr(undim(x), start, stop), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("chartr", c(old = "ANY", new = "ANY", x = "BumpyCharacterMatrix"), function(old, new, x) {
    BumpyMatrix(chartr(old, new, undim(x)), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("toupper", "BumpyCharacterMatrix", function(x) {
    BumpyMatrix(toupper(undim(x)), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("tolower", "BumpyCharacterMatrix", function(x) {
    BumpyMatrix(tolower(undim(x)), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("sub", c("ANY", "ANY", "BumpyCharacterMatrix"),
    function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 
{
    BumpyMatrix(sub(pattern, replacement, undim(x), ignore.case, perl, fixed, useBytes), dim(x), dimnames=dimnames(x))                     
})

#' @export
setMethod("gsub", c("ANY", "ANY", "BumpyCharacterMatrix"),
    function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    BumpyMatrix(gsub(pattern, replacement, undim(x), ignore.case, perl, fixed, useBytes), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("grepl", c("ANY", "BumpyCharacterMatrix"),
    function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 
{
    BumpyMatrix(grepl(pattern, undim(x), ignore.case, perl, fixed, useBytes), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("grep", c("ANY", "BumpyCharacterMatrix"),
    function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
{
    BumpyMatrix(grep(pattern, undim(x), ignore.case, perl, value, fixed, useBytes, invert), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("startsWith", c("BumpyCharacterMatrix", "ANY"), function(x, prefix) {
    BumpyMatrix(startsWith(undim(x), prefix), dim(x), dimnames=dimnames(x))
})

#' @export
setMethod("endsWith", c("BumpyCharacterMatrix", "ANY"), function(x, suffix) {
    BumpyMatrix(endsWith(undim(x), suffix), dim(x), dimnames=dimnames(x))
})