#' Converts json string to dataframe with columns 'value' and 'count'
#'
#' @param incoming json string with ids and values
#' @param bin_split splitting character between bins
#' @param id_split splitting character between id and bins
#' @export
#' @return dataframe of ids and values
#' @examples
#'    string_to_bin('{"0": 1, "1": 10, "2": 30}')
string_to_bin <- function (incoming, bin_split=",", id_split=":") {
  # ensure string, remove spaces
  incoming <- gsub(" ", "", toString(incoming), fixed=TRUE)
  # remove quotes
  incoming <- gsub("'", "", incoming, fixed = TRUE)
  # remove brackets
  incoming <- substr(incoming, 2, nchar(incoming)-1)
  # separate bins
  bins <- unlist(strsplit(incoming,bin_split))
  # seperate id and value
  idVal_pair <- mapply(FUN=strsplit, bins, c(id_split), USE.NAMES = FALSE)
  # get ids as numbers
  ids <- as.numeric(unlist(lapply(idVal_pair, '[[', 1)))
  # get vals as integers
  vals <- as.integer(unlist(lapply(idVal_pair, '[[', 2)))
  # return dataframe
  data.frame(id = ids, count = vals)
}


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
      runif(count, min = id - width/2, max = id + width/2)
    }
  } else {
    add_bin <- function(id, count) {
      rep(id, count)
    }
  }
  as.vector(unlist(mapply(FUN=add_bin, idVec, count=bins$count)))
}
