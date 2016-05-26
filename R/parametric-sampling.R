#' Monte Carlo Parametric Sampling
#'
#' @param distribution - name of distribution from which to sample
#' @param param_list list or vector of parameters for distribution
#' @return function taking a size argument to return number of samples
#' @export
#' @examples
#'   sample_MC_from_dist("normal", list(0, 1))
#'   sample_MC_from_dist("normal", list(mean = 0, sd = 1))
#'   sample_MC_from_dist("hyper", list(m = 10, n = 7, k = 8))
#'   sample_MC_from_dist("hyper", list(10, 7, 8))(10)
sample_MC_from_dist <- function(distribution, param_list) {
  param_list <- Filter(Negate(is.null), param_list)
  function(m) {
    do.call(
      match.fun(paste0('r', convert_dist(distribution))),
      #append(param_list, list(n = m))
      append(m, as.list(param_list))
    )
  }
}

#' Latin Hypercube Parametric Sampling
#'
#' @inheritParams sample_MC_from_dist
#' @return vector of length n samples from distribution
#' @importFrom lhs randomLHS
#' @export
#' @examples
#'   sample_LH_from_dist("normal", c(0, 1))
#'   sample_LH_from_dist("normal", list(mean = 0, sd = 1))
sample_LH_from_dist <- function(distribution, param_list) {
  param_list <- Filter(Negate(is.null), param_list)
  function(m) {
    do.call(
      match.fun(paste0('q', convert_dist(distribution))),
      append(
        param_list,
        list(p = as.vector(lhs::randomLHS(m, 1)))
      )
    )
  }
}


#' Parametric Rejection Sampling
#'
#' @param distribution - name of distribution from which to sample
#' @param param_list list or vector of parameters for distribution
#' @param type "MC" for Monte Carlo or "LH" for Latin Hypercube
#' @param lower lower bound for samples
#' @param upper upper bound for samples
#' @return function taking int > 0 returning samples from specified distribution
#'   with parameters with bounds
#' @export
#' @examples
#'    rej_sample_from_dist("normal", list(mean = 0, sd = 1))(10)
#'    rej_sample_from_dist("normal", type = "LH",  c(0,1), lower = -1, upper = 1)(10)
rej_sample_from_dist <- function(distribution, param_list, type = "MC",
    lower = -Inf, upper = Inf) {
  sampling_function <- switch(type,
    "MC" = sample_MC_from_dist,
    "LH" = sample_LH_from_dist,
    stop("Not a valid sampling type")
  )
  sampling_function <- sampling_function(distribution = distribution, param_list = param_list)
  function(m) {
    accepted <- c()
    difference <- m
    while (difference > 0) {
      generated <- sampling_function(difference)
      generated <- generated[generated >= lower]
      accepted <- c(accepted, generated[generated <= upper])
      difference <- m - length(accepted)
    }
    accepted
  }
}
