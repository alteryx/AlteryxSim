#' Converts binned data into vector of data
#'
#' @param bins dataframe with columns for id and count
#' @param discrete whether the data is discrete (default is False)
#' @return convert bins into vector of data
#' @export
bin_to_data <- function (bins, discrete = FALSE) {
  idVec <- bins$id
  if(!discrete) {
    idVec <- as.numeric(idVec)
    diffs <- diff(idVec)
    if(any(diff(diffs)!=0)) {
      stop("bins must have equal width")
    }
    width <- diffs[1]
    add_bin <- function (id, count) {
      runif(count, min = id, max = id + width)
    }
  } else {
    add_bin <- function(id, count) {
      rep(id, count)
    }
  }
  as.vector(unlist(mapply(FUN=add_bin, idVec, count=bins$count)))
}
