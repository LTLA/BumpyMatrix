.create_sparse_bumpy_matrix <- function(x, dim, dimnames=NULL) {
    chosen <- sort(sample(dim[1] * dim[2], length(x)))
    out <- Matrix::sparseMatrix(integer(0), integer(0), x=numeric(0), dim=dim, dimnames=dimnames)
    out[chosen] <- seq_along(chosen)
    BumpyMatrix(x, proxy=out)
}

.promote_to_dense <- function(x) {
    chosen <- which(x@proxy!=0)
    all.na <- rep(NA_integer_, nrow(x)*ncol(x))
    all.na[chosen] <- x@proxy[chosen]
    x@data <- BumpyMatrix:::.expand_List(x@data, all.na)
    x@proxy <- matrix(seq_len(prod(dim(x))), nrow(x), ncol(x), dimnames=dimnames(x))
    x
}
