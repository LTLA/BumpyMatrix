# This tests the functionality of the DFrameMatrix.
# library(testthat); library(BumpyMatrix); source("setup.R"); source("test-dframe.R")

library(S4Vectors)
df <- DataFrame(x=runif(100), y=runif(100))
f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
out <- split(df, f)
names(out) <- NULL
mat <- BumpyMatrix(out, c(5, 4))

test_that("DF fields operations work as expected", {
    expect_identical(commonColnames(mat), c("x", "y"))
    commonColnames(mat) <- c("X", "Y")
    expect_identical(commonColnames(mat), c("X", "Y"))
})

test_that("DF basic subsetting works as expected", {
    # First, some cursory checks.
    expect_identical(mat[,1], head(out, 5))
    expect_identical(mat[1,], out[1 + (0:3) * nrow(mat)])
    expect_identical(undim(mat[,1,drop=FALSE]), head(out, 5)) # same with drop=FALSE.
    expect_identical(undim(mat[1,,drop=FALSE]), out[1 + (0:3) * nrow(mat)])

    # Now trying out the k.
    expect_identical(mat[,,"x"], BumpyMatrix(out[,"x"], c(5,4)))
    invert <- mat[,,c("y", "x")]
    expect_identical(commonColnames(invert), c("y", "x"))

    # Trying out the 'k' with more complex drops.
    expect_identical(mat[,1,"x"], head(out[,"x"], 5))
    expect_identical(mat[,1,"x",.dropk=FALSE], head(out[,"x",drop=FALSE], 5))

    expect_identical(undim(mat[,1:2,"x"]), head(out[,"x"], 10))
    expect_identical(undim(mat[,1:2,"x",.dropk=FALSE]), head(out[,"x",drop=FALSE], 10))
})

test_that("DF BumpyMatrix subsetting continues to work as expected", {
    sub <- mat[mat[,,'x'] > 0.5 & mat[,,'y'] > 0.5]
    expect_identical(undim(sub), out[out[,'x'] > 0.5 & out[,'y'] > 0.5])
})

test_that("DF subset replacement works as expected", {
    # First, some cursory checks.
    copy <- mat
    copy[,4] <- copy[,3,drop=FALSE]
    expect_identical(copy[,3], copy[,4])

    copy <- mat
    copy[1,] <- copy[2,,drop=FALSE]
    expect_identical(copy[1,], copy[2,])

    # Now onto the behavior with 'k'.
    copy <- mat
    copy[,,'x'] <- copy[,,'x'] * 2
    expect_identical(undim(copy[,,'x']), out[,'x']*2)

    copy <- mat
    copy[,,'x'] <- copy[,,'y',drop=FALSE] # same with DFrameMatrix assignment.
    expect_identical(undim(copy[,,'x']), out[,'y'])

    copy <- mat
    copy[,,c('x', 'y')] <- copy[,,c('y', 'x'),drop=FALSE] 
    expect_identical(undim(copy[,,'x']), out[,'y'])
    expect_identical(undim(copy[,,'y']), out[,'x'])

    # More tests with 'k' plus indexing.
    copy <- mat
    copy[,1,'x'] <- copy[,1,'x',.dropk=TRUE, drop=FALSE] * 2
    expect_identical(head(undim(copy[,,'x']), 5), head(out[,'x']*2, 5))
    expect_identical(undim(copy[,,'x'])[6:10], out[,'x'][6:10])

    copy <- mat
    copy[1,,'x'] <- copy[1,,'x',.dropk=TRUE, drop=FALSE] * 2
    idx <- 1 + (0:3) * nrow(mat)
    expect_identical(undim(copy[,,'x'])[idx], out[,'x'][idx]*2)
    idx <- 2 + (0:3) * nrow(mat)
    expect_identical(undim(copy[,,'x'])[idx], out[,'x'][idx])
})

test_that("everything works with sparse proxies", {
    smat <- .create_sparse_bumpy_matrix(out[1:8], c(5, 4))
    ref.smat <- .promote_to_dense(smat)

    # Subsetting works as expected.
    expect_identical(smat[,1], ref.smat[,1])
    expect_identical(smat[1,], ref.smat[1,])
    expect_identical(smat[,,c("y", "x")][,1], ref.smat[,,c("y", "x")][,1])
    expect_identical(smat[,,"x"][,1], ref.smat[,,"x"][,1])
              
    # Subset replacement works as expected.
    copy <- smat
    copy[,,"x"] <- ref.smat[,,"y"]
    expect_identical(copy[,,"x"][,1], ref.smat[,,"y"][,1])

    copy <- smat
    copy[1,,"x"] <- ref.smat[1,,"y", drop=FALSE]
    expect_identical(copy[1,,"x"], ref.smat[1,,"y"])
    expect_identical(copy[2,,"x"], ref.smat[2,,"x"])

    copy <- smat
    copy[,1,"x"] <- ref.smat[,1,"y", drop=FALSE]
    expect_identical(copy[,1,"x"], ref.smat[,1,"y"])
    expect_identical(copy[,2,"x"], ref.smat[,2,"x"])

    copy <- smat
    copy[1,1,"x"] <- ref.smat[1,1,"y", drop=FALSE]
    expect_identical(copy[1,1,"x"], ref.smat[1,1,"y"])
    expect_identical(copy[1,2,"x"], ref.smat[1,2,"x"])
})
