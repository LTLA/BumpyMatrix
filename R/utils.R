.create_empty_List <- function(data) {
    if (length(data)) {
        empty <- data[1]
    } else {
        empty <- as(list(unlist(data)), "CompressedList")
    }
    empty[IntegerList(0)]
}

.expand_List <- function(data, i) {
    lost <- is.na(i) | i==0
    if (!any(lost)) {
        output <- data[i]
    } else {
        empty <- .create_empty_List(data)
        combined <- c(data, empty)
        i[lost] <- length(combined)
        output <- combined[i]
    }
    output
}

.reorder_indices <- function(x) {
    out <- .reorder_indices_raw(x@proxy, x@data)
    x@proxy <- out[[1]]
    x@data <- out[[2]]
    x
}

.reorder_indices_raw <- function(proxy, data) {
    nzero <- which(proxy!=0)
    indices <- proxy[nzero]
    proxy[nzero] <- seq_along(nzero)
    data <- data[indices]
    list(proxy, data) 
}

.increment_indices <- function(proxy, offset) {
    nzero <- which(proxy!=0)
    proxy[nzero] <- proxy[nzero] + offset
    proxy
}

.reconcile_matrices <- function(values)
# Forces all of them to have the same non-zero elements in 'proxy'.
# Note, we only have to check whether the non-zero indices are the same,
# as the enforced consecutive nature means that the non-zero values must be the same.
{
    all.matrices <- TRUE
    for (v in values) {
        if (!is.matrix(v@proxy)) {
            all.matrices <- FALSE
            break
        }
    }
    if (all.matrices) {
        return(values)
    }

    ref.nzero <- which(values[[1]]@proxy!=0)
    refreshed <- vector("list", length(values))
    refreshed[[1]] <- ref.nzero

    okay <- TRUE
    for (i in seq_along(values)[-1]) {
        cur.nzero <- which(values[[i]]@proxy!=0)
        refreshed[[i]] <- cur.nzero
        if (!identical(cur.nzero, ref.nzero)) {
            okay <- FALSE
        }
    }
    if (okay) {
        return(values)
    }

    combined <- sort(unique(unlist(refreshed)))
    for (i in seq_along(values)) {
        current <- values[[i]]
        nzero <- refreshed[[i]]
        indices <- current@proxy[nzero]
        current@data <- .expand_List(current@data[indices], match(combined, nzero))
        current@proxy[combined] <- seq_along(combined)
        values[[i]] <- current
    }

    values
}

.is_sparse <- function(x) {
    if (!is.matrix(x)) {
        if (length(undim(x)) != as.double(nrow(x)) * ncol(x)) {
            return(TRUE)
        }
    }
    FALSE
}

.spawn_empty_shell <- function(x, empty) {
    N <- as.double(nrow(x)) * ncol(x)
    if (length(empty)==1) {
        rep(empty, N)
    } else {
        matrix(empty, N, length(empty), byrow=TRUE)
    }
}

.create_empty_entry <- function(x) {
    .create_empty_List(undim(x))
}
