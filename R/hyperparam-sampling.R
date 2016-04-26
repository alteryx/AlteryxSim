#' Quantile function sampling
#'
#' @param distribution distribution to sample from
#' @param quantile_vector vector of quantiles
#' @param param_list list or vector of parameters
#' @return vector of sampled data points
#' @export
#' @examples
#'    quantile_sample("normal", c(0,.25,.5,.75,1), list(mean = 0, sd = 1))
#'    quantile_sample("lognormal", seq(0,1,by=.1), list(meanlog = 0, sdlog = 1))
quantile_sample <- function(distribution, quantile_vector, param_list) {
  # Quantile function sampling
  param_list <- Filter(Negate(is.null), param_list)
  fun_params <- append(param_list, list(p = quantile_vector))
  dist_fun <- match.fun(paste0('q', convert_dist(distribution)))
  do.call(dist_fun, fun_params)
}

#' Samples flexible parameters from distribution
#'
#'
#'
#' @param distribution name of distribution
#' @param quantile_count desired number of equidistant quantiles
#' @param param_list first parameter for 'distribution' - params for distribution
#' @param inf.remove if TRUE, removes +Inf and -Inf value from resulting vector
#' @return Vector of length quantile_count, quantile_count - 1, or quantile_count - 2,
#'    depending on inf.remove and number of inf values
#' @export
#' @examples
#'  sample_hyper(distribution = "normal", quantile_count = 11, param_list =
#'  list(mean = 0, sd = 1))
sample_hyper <- function(distribution = "uniform", quantile_count,
    param_list = list(min = 0, max = 1), inf.remove = TRUE) {
  quantiles <- seq(0, 1, length.out = quantile_count)
  results_vector <- quantile_sample(distribution = distribution, quantile_vector = quantiles, param_list = param_list)
  if (inf.remove) {
    results_vector <- subset(results_vector, !is.infinite(results_vector))
  }
  results_vector
}


#' Joins hyperparameter to existing data
#'
#'
#'
#' @param data data to join to hyperparam
#' @param name  name of flexible paramter to cartesian product with
#' @param ... additional arguments to pass to \code{sample_hyper}
#' @return
#'    Vector of length quantile_count, quantile_count - 1, or quantile_count - 2,
#'    depending on inf.remove and number of inf values
sample_hyper_join <- function(data, name, ...) {
  hyper_vector <- sample_hyper(...)
  df.expanded <- data[rep(seq.int(1,nrow(data)), length(hyper_vector)), 1:ncol(data)]
  df.expanded[, name] <- rep(hyper_vector, nrow(data))
  df.expanded
}
