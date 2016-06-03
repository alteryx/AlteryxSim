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
  x <- unname(dist_conversion_vector[distribution])
  if(is.na(x)) {
    distribution
  } else {
    x
  }
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

stop <- function(...){
  if ("AlteryxRDataX" %in% search()){
    AlteryxRDataX::stop.Alteryx(...)
  } else {
    base::stop(...)
  }
}

#' Generic check for illegal parameters; throws error if params out of bounds
#' 
#' @param paramList list of parameters
#' @export
errorCheckParams <- function(paramList) {
  UseMethod('errorCheckParams')
}

#' Default error check; returns no error
#' 
#' @inheritParams errorCheckParams
#' @export
errorCheckParams.default <- function(paramList) {}

#' uniform distribution error check
#' 
#' @inheritParams errorCheckParams
#' @export
errorCheckParams.uniform <- function(paramList) {
  if (paramList$max <= paramList$min) {
    stop("For uniform distribution, min must be less than max")
  }
}

#' triangular distribution error check
#' 
#' @inheritParams errorCheckParams
#' @export
errorCheckParams.triangular <- function(paramList) {
  if (paramList$a > paramList$c || paramList$b < paramList$c || paramList$b <= paramList$a) {
    stop("For triangular distribution, must have min <= most likely <= max and min < max")
  }
}

#' Generic error check for illegal bounds; throws error if set bound is outside of absolute bound
#' 
#' @param boundsVec vector of bounds
#' @export
errorCheckBounds <- function(boundsVec) {
  UseMethod("errorCheckBounds")
}

#' Default error check for bounds method
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.default <- function(boundsVec) {}

#' Error check for binomial out of bounds
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.binomial <- function(boundsVec) {
  if(boundsVec[2] <= 0) {
    stop("Absolute lower bound for binomial is 0")
  }
}

#' Error check for gamma out of bounds
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.gamma <- function(boundsVec) {
  if(boundsVec[2] <= 0) {
    stop("Absolute lower bound for gamma is 0")
  }
}

#' Error check for lognormal out of bounds
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.lognormal <- function(boundsVec) {
  if(boundsVec[2] <= 0) {
    stop("Absolute lower bound for lognormal is 0")
  }
}

#' Error check for pareto out of bounds
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.pareto <- function(boundsVec) {
  if(boundsVec[2] <= 0) {
    stop("Absolute lower bound for pareto is 0")
  }
}

#' Error check for poisson out of bounds
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.poisson <- function(boundsVec) {
  if(boundsVec[2] <= 0) {
    stop("Absolute lower bound for poisson is 0")
  }
}

#' Error check for geometric out of bounds
#' 
#' @inheritParams errorCheckBounds
#' @export
errorCheckBounds.geometric <- function(boundsVec) {
  if(boundsVec[2] <= 0) {
    stop("Absolute lower bound for geometric is 0")
  }
}