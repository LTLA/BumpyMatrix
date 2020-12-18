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
#' The exception is for \link{Summary} methods like \code{max} and \code{min};
#' these return an ordinary matrix where each entry contains a scalar value for the corresponding entry of \code{x}.
#' Furthermore, \code{range} will return a 3-dimensional array containing the minimum and maximum for each entry of \code{x}.
#' 
#' Common mathematical operations are implemented that apply to each entry of the BumpyAtomicMatrix:
#' \itemize{
#' \item \code{mean}, \code{sd}, \code{median}, \code{mad}, \code{var} and \code{IQR} take a single BumpyAtomicMatrix
#' and return an ordinary double-precision matrix of the same dimensions containing the computed statistic for each entry of the input.
#' This is possible as all operations are guaranteed to produce a scalar.
#' \item \code{quantile} takes a single BumpyAtomicMatrix as input and return a 3-dimensional array.
#' The first dimension contains the requested quantiles, the second dimension corresponds to the rows of \code{x} 
#' and the third dimension corresponds to the columns of \code{x}.
#' \item \code{which.max} and \code{which.min} take a single BumpyAtomicMatrix
#' and return an ordinary integer matrix of the same dimensions containing the index for the min/max value per entry.
#' (This is set to \code{NA} if the entry of the input has length zero.)
#' \item \code{pmin}, \code{pmax}, \code{pmin.int} and \code{pmax.int} take multiple BumpyAtomicMatrix objects of the same dimensions,
#' and return a BumpyAtomicMatrix containing the result of running the same function across corresponding entries of the input objects.
#' \item \code{cor}, \code{cov} and (optionally) \code{var} take two BumpyAtomicMatrix objects of the same dimensions,
#' and return an ordinary matrix containing the computed statistic for the corresponding entries of the inputs. 
#' This is possible as all operations are guaranteed to produce a scalar.
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
#' x <- splitAsList(runif(1000), factor(sample(50, 1000, replace=TRUE), 1:50))  
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
#' x <- splitAsList(sample(LETTERS, 100, replace=TRUE), 
#'     factor(sample(20, 100, replace=TRUE), 1:20))
#' cmat <- BumpyMatrix(x, c(5, 4))
#' cmat[,1]
#'
#' tolower(cmat[,1])
#' grepl("A|E|I|O|U", cmat)[,1]
#' sub("A|E|I|O|U", "vowel", cmat)[,1]
#' 
#' @aliases
#' BumpyAtomicMatrix-class
#' show,BumpyAtomicMatrix-method
#' BumpyIntegerMatrix-class
#' BumpyCharacterMatrix-class
#' BumpyNumericMatrix-class
#' BumpyLogicalMatrix-class
#' Ops,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' Ops,atomic,BumpyAtomicMatrix-method
#' Ops,BumpyAtomicMatrix,atomic-method
#' Ops,matrix,BumpyAtomicMatrix-method
#' Ops,BumpyAtomicMatrix,matrix-method
#' Ops,BumpyAtomicMatrix,missing-method
#' !,BumpyAtomicMatrix-method
#' Math,BumpyAtomicMatrix-method
#' Math2,BumpyAtomicMatrix-method
#' Summary,BumpyAtomicMatrix-method
#' range,BumpyAtomicMatrix-method
#'
#' mean,BumpyAtomicMatrix-method
#' sd,BumpyAtomicMatrix-method
#' median,BumpyAtomicMatrix-method
#' mad,BumpyAtomicMatrix-method
#' var,BumpyAtomicMatrix,missing-method
#' IQR,BumpyAtomicMatrix-method
#' which.min,BumpyAtomicMatrix-method
#' which.max,BumpyAtomicMatrix-method
#' pmin,BumpyAtomicMatrix-method
#' pmax,BumpyAtomicMatrix-method
#' pmin.int,BumpyAtomicMatrix-method
#' pmax.int,BumpyAtomicMatrix-method
#' var,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' cov,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' cor,BumpyAtomicMatrix,BumpyAtomicMatrix-method
#' quantile,BumpyAtomicMatrix-method
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
#' @importFrom utils str
setMethod("show", "BumpyAtomicMatrix", function(object) {
    callNextMethod()
    if (nrow(object) && ncol(object)) {
        cat("preview [1,1]:\n")
        first <- object[1,1][[1]]
        cat(" ")
        str(first)
    }
})

#' @export
setMethod("Ops", c("BumpyAtomicMatrix", "BumpyAtomicMatrix"), function(e1, e2) {
    if (!identical(dim(e1), dim(e2))) {
        stop("'e1' and 'e2' must have the same dimensions")
    }
    recon <- .reconcile_matrices(list(e1, e2))
    e1 <- recon[[1]]
    e2 <- recon[[2]]
    out <- callGeneric(undim(e1), undim(e2))
    BumpyMatrix(out, proxy=e1@proxy)
})

#' @export
setMethod("Ops", c("atomic", "BumpyAtomicMatrix"), function(e1, e2) {
    if (length(e1) > 1 && .is_sparse(e2)) {
        indices <- ((which(e2@proxy > 0) - 1L) %% length(e1)) + 1L
        e1 <- e1[indices]
    }
    out <- callGeneric(e1, undim(e2))
    BumpyMatrix(out, proxy=e2@proxy)
})

#' @export
setMethod("Ops", c("matrix", "BumpyAtomicMatrix"), function(e1, e2) {
    if (!identical(dim(e1), dim(e2))) {
        stop("matrices should have same dimensions for binary operator")
    }
    if (.is_sparse(e2)) {
        e1 <- e1[which(e2@proxy > 0)]
    }
    out <- callGeneric(as.vector(e1), undim(e2))
    BumpyMatrix(out, proxy=e2@proxy)
})

#' @export
setMethod("Ops", c("BumpyAtomicMatrix", "atomic"), function(e1, e2) {
    if (length(e2) > 1 && .is_sparse(e1)) {
        indices <- ((which(e1@proxy > 0) - 1L) %% length(e2)) + 1L
        e2 <- e2[indices]
    }
    out <- callGeneric(undim(e1), e2)
    BumpyMatrix(out, proxy=e1@proxy)
})

#' @export
setMethod("Ops", c("BumpyAtomicMatrix", "matrix"), function(e1, e2) {
    if (!identical(dim(e1), dim(e2))) {
        stop("matrices should have same dimensions for binary operator")
    }
    if (.is_sparse(e1)) {
        e2 <- e2[which(e1@proxy > 0)]
    }
    out <- callGeneric(undim(e1), as.vector(e2))
    BumpyMatrix(out, proxy=e1@proxy)
})

#' @export
setMethod("Ops", c("BumpyAtomicMatrix", "missing"), function(e1, e2) {
    out <- callGeneric(undim(e1))
    BumpyMatrix(out, proxy=e1@proxy)
})

#' @export
setMethod("!", "BumpyAtomicMatrix", function(x) {
    BumpyMatrix(!undim(x), proxy=x@proxy)
})

#' @export
setMethod("Math", "BumpyAtomicMatrix", function(x) {
    out <- callGeneric(undim(x))
    BumpyMatrix(out, proxy=x@proxy)
})

#' @export
setMethod("Math2", "BumpyAtomicMatrix", function(x) {
    out <- callGeneric(undim(x))
    BumpyMatrix(out, proxy=x@proxy)
})

#' @export
setMethod("Summary", "BumpyAtomicMatrix", function(x) {
    out <- callGeneric(undim(x))

    if (.is_sparse(x)) {
        tmp <- .spawn_empty_shell(x, empty=callGeneric(.create_empty_entry(x)))
        tmp[which(x@proxy!=0)] <- out
        out <- tmp
    }

    matrix(out, nrow(x), ncol(x), dimnames=dimnames(x))
})

#' @export
setMethod("range", "BumpyAtomicMatrix", function(x) {
    out <- range(undim(x))

    if (.is_sparse(x)) {
        tmp <- .spawn_empty_shell(x, empty=range(.create_empty_entry(x)))
        tmp[which(x@proxy!=0),] <- out
        out <- tmp
    }

    array(out, c(dim(x), 2L), c(dimnames(x), list(NULL)))
})

#######################################################################
## The following section is derived from IRanges' AtomicList-utils.R ##
#######################################################################

.apply_matrix_out <- function(x, FUN, ...) {
    collected <- FUN(undim(x), ...)

    if (.is_sparse(x)) {
        tmp <- .spawn_empty_shell(x, empty=FUN(.create_empty_entry(x)))
        tmp[which(x@proxy!=0)] <- collected
        collected <- tmp
    }

    matrix(collected, nrow(x), ncol(x), dimnames=dimnames(x))
}

#' @export
setMethod("mean", "BumpyAtomicMatrix", function(x, ...) .apply_matrix_out(x, mean, ...))

#' @export
setMethod("sd", "BumpyAtomicMatrix", function(x, na.rm=TRUE) .apply_matrix_out(x, sd, na.rm=na.rm))

#' @export
setMethod("median", "BumpyAtomicMatrix", function(x, na.rm=TRUE) .apply_matrix_out(x, median, na.rm=na.rm))

#' @export
setMethod("mad", "BumpyAtomicMatrix", function(x, center=median(x), constant=1.4826, na.rm=FALSE, low=FALSE, high=FALSE) {
    if (!missing(center)) {
        stop("'center' argument is not currently supported")
    }
    .apply_matrix_out(x, mad, constant=constant, na.rm=na.rm, low=low, high=high)
})

#' @export
setMethod("var", c("BumpyAtomicMatrix", "missing"), function(x, y=NULL, na.rm=FALSE, use) {
    if (missing(use)) {
        use <- ifelse(na.rm, "na.or.complete", "everything")
    }
    .apply_matrix_out(x, var, na.rm=na.rm, use=use)
})

#' @export
setMethod("which.min", "BumpyAtomicMatrix", function(x) .apply_matrix_out(x, which.min))

#' @export
setMethod("which.max", "BumpyAtomicMatrix", function(x) .apply_matrix_out(x, which.max))

#' @export
setMethod("IQR", "BumpyAtomicMatrix", function(x, na.rm=FALSE, type=7) .apply_matrix_out(x, IQR, na.rm=na.rm, type=type))

#' @export
setMethod("quantile", "BumpyAtomicMatrix", function(x, ...) {
    out <- quantile(undim(x), ...) 

    if (.is_sparse(x)) {
        tmp <- t(.spawn_empty_shell(x, empty=quantile(.create_empty_entry(x))))
        rownames(tmp) <- rownames(out)
        tmp[,which(x@proxy!=0)] <- out
        out <- tmp
    }

    array(out, c(nrow(out), dim(x)), c(list(rownames(out)), dimnames(x)))
})

.apply_matrix_out_pm <- function(..., FUN, MoreArgs=list()) {
    args <- list(...)
    if (length(unique(lapply(args, dim)))!=1L) {
        stop("all objects in '", substitute(FUN), "(<BumpyAtomicMatrix>)' should have the same 'dim'")
    }

    args <- .reconcile_matrices(args)
    output <- lapply(args, undim)
    collected <- do.call(FUN, c(output, MoreArgs))
    BumpyMatrix(collected, proxy=args[[1]]@proxy)
}

#' @export
setMethod("pmax", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out_pm(..., FUN=pmax, MoreArgs = list(na.rm = na.rm)))

#' @export
setMethod("pmax.int", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out_pm(..., FUN=pmax.int, MoreArgs = list(na.rm = na.rm)))

#' @export
setMethod("pmin", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out_pm(..., FUN=pmin, MoreArgs = list(na.rm = na.rm)))

#' @export
setMethod("pmin.int", "BumpyAtomicMatrix", function(..., na.rm = FALSE) .apply_matrix_out_pm(..., FUN=pmin.int, MoreArgs = list(na.rm = na.rm)))

.apply_matrix_out_dual <- function(x, y, FUN, ...) {
    if (!identical(dim(x), dim(y))) {
        stop("'x' and 'y' must have the same dimensions")
    }
    recon <- .reconcile_matrices(list(x, y))
    x <- recon[[1]]
    y <- recon[[2]]
    collected <- FUN(undim(x), undim(y), ...)

    if (.is_sparse(x)) {
        tmp <- .spawn_empty_shell(x, empty=FUN(.create_empty_entry(x), .create_empty_entry(y), ...))
        tmp[which(x@proxy!=0)] <- collected
        collected <- tmp
    }

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
    out <- unstrsplit(undim(x), sep=sep)

    if (.is_sparse(x)) {
        tmp <- .spawn_empty_shell(x, empty=unstrsplit(.create_empty_entry(x)))
        tmp[which(x@proxy!=0)] <- out
        out <- tmp
    }

    matrix(out, nrow(x), ncol(x), dimnames=dimnames(x))              
})

#' @export
setMethod("nchar", "BumpyCharacterMatrix", function(x) {
    BumpyMatrix(nchar(undim(x)), proxy=x@proxy)
})

#' @export
setMethod("substring", "BumpyCharacterMatrix", function(text, first, last = 1000000L) {
    BumpyMatrix(substring(undim(text), first, last), proxy=text@proxy)
})

#' @export
setMethod("substr", "BumpyCharacterMatrix", function(x, start, stop) {
    BumpyMatrix(substr(undim(x), start, stop), proxy=x@proxy)
})

#' @export
setMethod("chartr", c(old = "ANY", new = "ANY", x = "BumpyCharacterMatrix"), function(old, new, x) {
    BumpyMatrix(chartr(old, new, undim(x)), proxy=x@proxy)
})

#' @export
setMethod("toupper", "BumpyCharacterMatrix", function(x) {
    BumpyMatrix(toupper(undim(x)), proxy=x@proxy)
})

#' @export
setMethod("tolower", "BumpyCharacterMatrix", function(x) {
    BumpyMatrix(tolower(undim(x)), proxy=x@proxy)
})

#' @export
setMethod("sub", c("ANY", "ANY", "BumpyCharacterMatrix"),
    function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 
{
    BumpyMatrix(sub(pattern, replacement, undim(x), ignore.case, perl, fixed, useBytes), proxy=x@proxy)
})

#' @export
setMethod("gsub", c("ANY", "ANY", "BumpyCharacterMatrix"),
    function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
{
    BumpyMatrix(gsub(pattern, replacement, undim(x), ignore.case, perl, fixed, useBytes), proxy=x@proxy)
})

#' @export
setMethod("grepl", c("ANY", "BumpyCharacterMatrix"),
    function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 
{
    BumpyMatrix(grepl(pattern, undim(x), ignore.case, perl, fixed, useBytes), proxy=x@proxy)
})

#' @export
setMethod("grep", c("ANY", "BumpyCharacterMatrix"),
    function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
{
    BumpyMatrix(grep(pattern, undim(x), ignore.case, perl, value, fixed, useBytes, invert), proxy=x@proxy)
})

#' @export
setMethod("startsWith", c("BumpyCharacterMatrix", "ANY"), function(x, prefix) {
    BumpyMatrix(startsWith(undim(x), prefix), proxy=x@proxy)
})

#' @export
setMethod("endsWith", c("BumpyCharacterMatrix", "ANY"), function(x, suffix) {
    BumpyMatrix(endsWith(undim(x), suffix), proxy=x@proxy)
})
