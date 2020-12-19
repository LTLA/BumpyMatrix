# This tests special operations for BumpyAtomicMatrix objects.
# library(testthat); library(BumpyMatrix); source("setup.R"); source("test-atomic.R")

set.seed(88888000)
library(IRanges)
f <- factor(sample(20, 50, replace=TRUE), 1:20)

x <- NumericList(split(runif(50), f))  
x <- unname(x)
mat <- BumpyMatrix(x, c(5,4))

x2 <- NumericList(split(runif(50), f))
x2 <- unname(x2)
mat2 <- BumpyMatrix(x2, c(5,4))

smat <- .create_sparse_bumpy_matrix(x[1:10], c(5, 4))
ref.smat <- .promote_to_dense(smat)

test_that("Ops work correctly", {
    # Unary operations.
    expect_identical(mat, +mat)
    expect_identical(-mat, 0-mat)
    expect_identical(!mat, mat==0)

    # Dual operations
    output <- mat + mat2
    expect_identical(output[,1], mat[,1] + mat2[,1])

    output <- mat2 > mat
    expect_s4_class(output, "BumpyLogicalMatrix")
    expect_identical(output[,1], mat[,1] < mat2[,1])

    expect_error(mat + mat[,1:2], "same dimensions")

    # Repeating this with sparse (or sparse + dense) proxies.
    output <- smat + smat
    expect_identical(output[,1], smat[,1] + smat[,1])
    output <- smat + ref.smat
    expect_identical(output[,1], ref.smat[,1] + ref.smat[,1])

    # One-sided ops.
    output <- mat > 0.5
    expect_identical(output[,1], mat[,1] > 0.5)
    output <- 1 + mat 
    expect_identical(output[,1], 1 + mat[,1])

    # ... with sparse matrices.
    output <- smat > 0.5
    expect_identical(output[,1], smat[,1] > 0.5)
    output <- 1 + smat 
    expect_identical(output[,1], 1 + smat[,1])
})

test_that("Math works correctly", {
    output <- log(mat)
    expect_identical(output[,1], log(mat[,1]))

    output <- round(mat)
    expect_identical(output[,1], round(mat[,1]))

    # For the sparse case.
    output <- log(smat)
    expect_identical(output[,1], log(smat[,1]))

    output <- 0 - smat
    expect_identical(output[,1], 0 - smat[,1])
})

test_that("Ops work correctly for atomic vectors/matrices", {
    X <- seq_len(nrow(mat))
    mat2 <- mat + X
    expect_identical(mat[1,1][[1]]+1, mat2[1,1][[1]])
    expect_identical(mat[5,1][[1]]+5, mat2[5,1][[1]])
    expect_identical(mat[1,4][[1]]+1, mat2[1,4][[1]])
    expect_identical(mat[5,4][[1]]+5, mat2[5,4][[1]])

    mat2b <- X + mat
    expect_identical(mat2, mat2b)

    # Recycles correctly?
    mat2 <- mat + 1:2
    expect_identical(mat[1,1][[1]]+1, mat2[1,1][[1]])
    expect_identical(mat[5,1][[1]]+1, mat2[5,1][[1]])
    expect_identical(mat[1,4][[1]]+2, mat2[1,4][[1]])
    expect_identical(mat[5,4][[1]]+2, mat2[5,4][[1]])

    # Sparse case works correctly.
    expect_identical(ref.smat + X, .promote_to_dense(smat + X))
    expect_identical(X + ref.smat, .promote_to_dense(X + smat))

    # Trying with matrices.
    X <- matrix(seq_along(mat), nrow(mat), ncol(mat))
    mat3 <- mat + X
    expect_identical(mat[1,1][[1]]+1, mat3[1,1][[1]])
    expect_identical(mat[5,1][[1]]+5, mat3[5,1][[1]])
    expect_identical(mat[1,4][[1]]+16, mat3[1,4][[1]])
    expect_identical(mat[5,4][[1]]+20, mat3[5,4][[1]])

    mat3b <- X + mat
    expect_identical(mat3, mat3b)

    expect_error(X + mat[1:2,], "binary operator")
    expect_error(mat[1:2,] + X, "binary operator")

    # Sparse case works correctly.
    expect_identical(ref.smat + X, .promote_to_dense(smat + X))
    expect_identical(X + ref.smat, .promote_to_dense(X + smat))
})

test_that("Summary works correctly", {
    # Most operations emit an ordinary matrix.
    output <- max(mat)
    expect_identical(dim(output), dim(mat))
    expect_identical(as.vector(output), max(undim(mat)))

    # Except for range(), which emits a three-dimensional array.
    output <- range(mat)
    expect_identical(output[,,1], unname(min(mat)))
    expect_identical(output[,,2], unname(max(mat)))

    # Same for the sparse case.
    expect_identical(max(smat), max(ref.smat))
    expect_identical(range(smat), range(ref.smat))
})

test_that("numeric functions that emit matrices work correctly", {
    COMPARE <- function(output, ref, type="double") {
        expect_type(output, type)
        expect_identical(dim(output), dim(mat))
        expect_identical(as.vector(output), ref)
    }

    COMPARE(mean(mat), mean(undim(mat)))
    COMPARE(median(mat), median(undim(mat)))
    COMPARE(var(mat), var(undim(mat)))
    COMPARE(mad(mat), mad(undim(mat)))
    COMPARE(sd(mat), sd(undim(mat)))

    COMPARE(var(mat, mat2), var(undim(mat), undim(mat2)))
    COMPARE(cov(mat, mat2), cov(undim(mat), undim(mat2)))
    COMPARE(cor(mat, mat2), cor(undim(mat), undim(mat2)))

    COMPARE(which.max(mat), which.max(undim(mat)), type="integer")
    COMPARE(which.min(mat), which.min(undim(mat)), type="integer")

    # Technically, this emits a 3D array.
    out <- quantile(mat)
    expect_identical(out[,1,], quantile(mat[,1]))
    expect_identical(out[4,,], quantile(mat[4,]))

    out <- quantile(mat, p=0.5)
    expect_identical(out[,1,], drop(quantile(mat[,1], p=0.5)))
    expect_identical(out[4,,], drop(quantile(mat[4,], p=0.5)))

    # Same for the sparse case.
    expect_identical(mean(smat), mean(ref.smat))
    expect_identical(cov(smat, ref.smat), cov(ref.smat, ref.smat)) 
    expect_identical(which.max(smat), which.max(ref.smat))

    expect_identical(quantile(smat), quantile(ref.smat))
    expect_identical(quantile(smat, p=0.5), quantile(ref.smat, p=0.5))
})

test_that("numeric functions that emit BumpyMatrices work correctly", {
    COMPARE <- function(output, ref) {
        expect_identical(dim(output), dim(mat))
        expect_identical(undim(output), ref)
    }

    COMPARE(pmax(mat, mat2), pmax(undim(mat), undim(mat2)))
    COMPARE(pmin(mat, mat2), pmin(undim(mat), undim(mat2)))
    COMPARE(pmax.int(mat, mat2), pmax.int(undim(mat), undim(mat2)))
    COMPARE(pmin.int(mat, mat2), pmin.int(undim(mat), undim(mat2)))

    # Handles sparse and sparse/dense hybrids.
    expect_identical(pmax(smat, smat*2), smat*2)
    expect_identical(pmin(smat, smat*2), smat)
    expect_identical(pmax(smat, ref.smat*2)[,1], (ref.smat*2)[,1])
    expect_identical(pmin(smat, ref.smat*2)[,1], ref.smat[,1])
})

test_that("string functions on BunmpyCharacterMatrices work correctly", {
    pokemon <- c("charizard", "charmander", "charmeleon", "squirtle", "blastoise",
        "wartortle", "bulbasaur", "venusaur", "ivysaur", "caterpie", "metapod", "butterfree",
        "weedle", "kakuna", "beedrill")
    x <- CharacterList(split(sample(pokemon, 50, replace=TRUE), factor(sample(12, 50, replace=TRUE), 1:12)))
    x <- unname(x)
    mat <- BumpyMatrix(x, c(4, 3))

    # unstrsplit is a special case.
    output <- unstrsplit(mat, ",")
    expect_type(output, "character")
    expect_identical(dim(output), dim(mat))
    expect_identical(as.vector(output), unstrsplit(x, ","))

    smat <- .create_sparse_bumpy_matrix(x[1:10], c(5, 4))
    output <- unstrsplit(smat, ",")
    expect_identical(unstrsplit(.promote_to_dense(smat), ","), output)

    COMPARE <- function(output, ref) {
        expect_identical(dim(output), dim(mat))
        expect_identical(undim(output), ref)
    }

    COMPARE(nchar(mat), nchar(undim(mat)))
    COMPARE(substring(mat, 1, 5), substring(undim(mat), 1, 5))
    COMPARE(substr(mat, 2, 5), substr(undim(mat), 2, 5))
    COMPARE(chartr("w", "W", mat), chartr("w", "W", undim(mat)))
    COMPARE(toupper(mat), toupper(undim(mat)))
    COMPARE(tolower(mat), tolower(undim(mat)))
    COMPARE(sub("saur", "SAUR", mat), sub("saur", "SAUR", undim(mat)))
    COMPARE(gsub("a", "A", mat), gsub("a", "A", undim(mat)))
    COMPARE(grep("char", mat), grep("char", undim(mat)))
    COMPARE(grepl("char", mat), grepl("char", undim(mat)))

    # This doesn't work right now.
#    COMPARE(startsWith("char", mat), startsWith("char", undim(mat)))
#    COMPARE(endsWith("saur", mat), endsWith("saur", undim(mat)))
})

