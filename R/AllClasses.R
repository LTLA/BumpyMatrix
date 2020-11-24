#' @export
setClass("BumpyMatrix", contains="VIRTUAL", slots=c(data="CompressedList", dim="integer", dimnames="list"))

#' @export
setClass("BumpyAtomicMatrix", contains="BumpyMatrix", slots=c(data="CompressedAtomicList"))

#' @export
setClass("BumpyIntegerMatrix", contains="BumpyAtomicMatrix", slots=c(data="CompressedIntegerList"))

#' @export
setClass("BumpyNumericMatrix", contains="BumpyAtomicMatrix", slots=c(data="CompressedNumericList"))

#' @export
setClass("BumpyLogicalMatrix", contains="BumpyAtomicMatrix", slots=c(data="CompressedLogicalList"))

#' @export
setClass("BumpyCharacterMatrix", contains="BumpyAtomicMatrix", slots=c(data="CompressedCharacterList"))

#' @export
setClass("BumpyDFrameMatrix", contains="BumpyMatrix", slots=c(data="CompressedSplitDFrameList"))
