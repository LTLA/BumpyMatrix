# This tests the splitAsBumpyMatrix function.
# library(testthat); library(BumpyMatrix); source("test-split.R")

library(S4Vectors)
df <- DataFrame(value=runif(1000), 
   row=sample(LETTERS, 1000, replace=TRUE),
   column=sample(10, 1000, replace=TRUE)
)

mat <- splitAsBumpyMatrix(df$value, df$row, df$column)

test_that("splitAsBumpyMatrix works as expected", {
    expect_identical(rownames(mat), LETTERS)    
    expect_identical(colnames(mat), as.character(1:10))    
    expect_identical(mat[1,1][[1]], df$value[df$row=="A" & df$column==1])
    expect_identical(mat["C",6][[1]], df$value[df$row=="C" & df$column==6])
    expect_identical(mat["Z",8][[1]], df$value[df$row=="Z" & df$column==8])
})

test_that("splitAsBumpyMatrix works for DF iputs", {
    mat <- splitAsBumpyMatrix(df[,'value',drop=FALSE], df$row, df$column)
    expect_s4_class(mat, "BumpyDataFrameMatrix")

    expect_identical(rownames(mat), LETTERS)    
    expect_identical(colnames(mat), as.character(1:10))    
    expect_identical(mat[1,1][[1]], df[df$row=="A" & df$column==1,'value',drop=FALSE])
    expect_identical(mat["C",6][[1]], df[df$row=="C" & df$column==6,'value',drop=FALSE])
    expect_identical(mat["Z",8][[1]], df[df$row=="Z" & df$column==8,'value',drop=FALSE])
})

test_that("splitAsBumpyMatrix works for sparse requests", {
    mat <- splitAsBumpyMatrix(df$value[1:100], df$row[1:100], df$column[1:100], sparse=TRUE)
    expect_s4_class(mat@proxy, "dgCMatrix")

    ref <- splitAsBumpyMatrix(df$value[1:100], df$row[1:100], df$column[1:100])
    expect_identical(ref[,1], mat[,1])
    expect_identical(ref[1,], mat[1,])
})

test_that("unsplitAsDataFrame works as expected", {
    o1 <- order(df$row, df$column, df$value)
    ref <- df[o1,c("row", "column", "value")]

    out <- unsplitAsDataFrame(mat)
    out$column <- as.integer(out$column)
    o2 <- order(out$row, out$column, out$value)
    out <- out[o2,]

    expect_identical(ref, out)

    # Works as expected for sparse matrices.
    sub <- df[1:100,]
    smat <- splitAsBumpyMatrix(sub$value, sub$row, sub$column, sparse=TRUE)
    sout <- unsplitAsDataFrame(smat)
    sout$column <- as.integer(sout$column)
    expect_identical(nrow(sout), 100L)
    expect_true(all(sout %in% sub))
    expect_true(all(sub %in% sout))
})

test_that("unsplitAsDatFrame works for DFs", {
    o1 <- order(df$row, df$column, df$value)
    ref <- df[o1,c("row", "column", "value")]

    mat <- splitAsBumpyMatrix(DataFrame(X=df$value), df$row, df$column)
    out <- unsplitAsDataFrame(mat)
    out$column <- as.integer(out$column)

    expect_identical(colnames(out)[3], "X")
    colnames(out)[3] <- "value"
    o2 <- order(out$row, out$column, out$value)
    expect_identical(ref, out[o2,])

    # Preserves any row names and metadata.
    rownames(df) <- paste0("WHEE", seq_len(nrow(df)))
    metadata(df)$X <- "Aaron"

    mat <- splitAsBumpyMatrix(df[,"value",drop=FALSE], df$row, df$column)
    out2 <- unsplitAsDataFrame(mat)
    out2$column <- as.integer(out2$column)

    ref <- df[o1,c("row", "column", "value")]
    expect_identical(ref, out2[o2,])
})
