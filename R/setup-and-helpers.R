#' Get continuous distributions supported by Alteryx tools
#'
#' @return vector of supported continuous distributions
#' @export
#' @examples
#'    Alteryx_distributions_continuous()
Alteryx_distributions_continuous <- function() {
  c("normal", "gamma", "lognormal", "uniform", "triangular", "pareto")
}

#' Get discrete distributions supported by Alteryx tools
#'
#' @return vector of supported discrete distributions
#' @export
#' @examples
#'    Alteryx_distributions_continuous()
Alteryx_distributions_discrete <- function() {
  c("geometric", "poisson", "binomial")
}

#' Get distributions supported by Alteryx tools
#'
#' @return vector of supported distributions
#' @export
#' @examples
#'    Alteryx_distributions()
Alteryx_distributions <- function() {
  c(Alteryx_distributions_continuous(), Alteryx_distributions_discrete())
}

#' Convert distribution name to R distribution function suffix
#'
#' @param distribution string (or vector of strings) - name of distribution
#' @return string (or vector of strings) - R-recognized distribution, has corresponding d|p|q|r functions
#' @export
#' @examples
#'    convert_dist("normal")
#'    convert_dist(c("normal", "gamma", "lognormal"))
convert_dist <- function(distribution) {
  dist_conversion_vector <- c(
    normal = "norm",
    gamma = "gamma",
    lognormal = "lnorm",
    pareto = "pareto",
    uniform = "unif",
    triangular = "triangle",
    geometric = "geom",
    poisson = "pois",
    binomial = "binom"
  )
  unname(dist_conversion_vector[distribution])
}

#' Give vector of chunk sizes
#'
#' @param chunk_size maximal size of chunk
#' @param total_size sum of chunk sizes
#' @return vector of chunk_sizes
#' @export
#' @examples
#'    get_chunk_sizes(2,5)
#'    get_chunk_sizes(5,2)
get_chunk_sizes <- function(chunk_size, total_size) {
  chunk_numbers <- 1:ceiling(total_size/chunk_size)
  no_to_size <- function(chunk_no) {
    min(chunk_size, total_size - (chunk_no-1)*chunk_size)
  }
  unlist(lapply(chunk_numbers, FUN = no_to_size))
}

# #' Decides whether substring from list box interface tool is a true or false element
# #'
# #' @param x string
# #' @return bool whether the string ends in true
# #' @export
# #' @examples
# #'    get.true("Normal=True")
# get.true <- Vectorize(function(x) {ifelse(substr(x,nchar(x)-3, nchar(x))=="True",TRUE, FALSE)})

# #' Splits string by an = sign
# #'
# #' @param x string to split
# #' @return split string
# #' @export
# #' @examples
# #'    split.equals("Normal=True")
# split.equals <- function(x) strsplit(x, "=")

# #' get true components of string from list box interfact tool
# #'
# #' @param string string to eval
# #' @return vector of string terms which equal true
# #' @export
# #' @examples
# #'    get_all_trues("Normal=True,Lognormal=False,Gamma=True")
# get_all_trues <- function (string) {
#   trues <- unlist(strsplit(string, ','))
#   sapply(split.equals(subset(trues, get.true(trues))), "[[", 1)
# }

#' Apply function to each chunk if in Alteryx. If outside, run on full size
#'
#' @param nOutput int >= 0; if 0, run outside of Alteryx; if = 0, denotes output number in Alteryx
#' @param total_size total size of data
#' @param chunk_size maximal size of chunk
#' @param names names
#' @return function
#' @export
doInChunks <- function(nOutput, total_size, chunk_size, names = NULL){
  function(f){
    chunk_sizes = get_chunk_sizes(chunk_size = chunk_size, total_size = total_size)
    if (nOutput > 0) {
      f2 <- function(x){
        df <- as.data.frame(f(x))
        if(!is.null(names)) {
          names(df) <- names
        }
        AlteryxRDataX::write.Alteryx(df, nOutput = nOutput)
      }
      lapply(chunk_sizes, FUN = f2)
    } else {
      f2 <- function(x) {
        f2 <- function(x){
          df <- as.data.frame(f(x))
          if(!is.null(names)) {
            names(df) <- names
          }
          df
      }
      rbind(lapply(chunk_sizes, FUN = f2))
    }
  }
  }
}
