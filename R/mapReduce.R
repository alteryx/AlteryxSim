#' Map reduce code to help with processing of chunks
#'
#' @param nameInput if data is coming from Alteryx, name of the input data is coming from
#' @param chunkSize maximal size of chunk
#' @param totalSize total size of data
#' @param data dataframe outside of Alterx, not dataframe if inside Alteryx
#' @return function taking two function arguments - a map function and a reduce function and applying them as expected
#' @export
#' @examples
#' \dontrun{
#'    mapReduce(NULL, 15, 100,
#'      data.frame(x = 1:100)
#'    )(sum, function(x){sum(unlist(x))})
#'    mapReduce(NULL, 15, 100,
#'      data.frame(x = 1:100)
#'    )(function(x) {sum(x^2)}, function(x) {sum(unlist(x))})
#' }
mapReduce <- function(nameInput, chunkSize, totalSize, data) {
  function(map, reduce) {
    chunkCount <- ceiling(totalSize/chunkSize)
    getData <- function(chunkNumber) {
      if (class(data) != "data.frame") {
        if(chunkNumber == 1) {
          AlteryxRDataX::read.Alteryx.First(nameInput, chunkSize)
        } else {
          AlteryxRDataX::read.Alteryx.Next(nameInput)
        }
      } else {
        data[((chunkNumber-1)*chunkSize+1):(min(totalSize, chunkSize*chunkNumber)),]
      }
    }
    processChunk <- function(chunkNumber) {
      map(getData(chunkNumber))
    }
    if(!is.null(reduce)) {
      mappings <- lapply(1:chunkCount, processChunk)
      reduce(mappings)
    } else {
      lapply(1:chunkCount, processChunk)
    }
  }
}
