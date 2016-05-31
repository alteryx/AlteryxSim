#' Convert a fit to corresponding chisq value
#'
#' Convert a fit
#'
#' @param fit_obj object fit by
#' @return chi squared statistic
#' @import fitdistrplus
#' @export
#' @examples
#' \dontrun{
#'   chi_sq(fit_best_single(rnorm(100), "normal"))
#'   chi_sq(fit_best_single(rnorm(100), "triangle"))
#' }
chi_sq <- function (fit_obj) {
  if(is.na(fit_obj) || is.null(fit_obj)) {
    return (Inf)
  } else if (class(fit_obj)=='fitdist') {
    return (fitdistrplus::gofstat(fit_obj)$chisq)
  } else if (fit_obj$distribution %in% convert_dist(Alteryx_distributions_continuous())){
    return (custom_chisq(fit_obj))
  } else if (fit_obj$distribution %in% convert_dist(Alteryx_distributions_discrete())){
    return (custom_chisq_discrete(fit_obj))
  } else {
    stop("unsupported distribution")
  }
}

#' Get chi squared value from MLE for custom mle function for continuous dists
#'
#'
#'
#' @param custom_mle named list containing 'data' vector, 'distribution' name,
#'   and 'param_list' list
#' @return chisq statistic with number of bins 1/4*length(data)^{3/5} -
#'   corresponding to average bin size used in fitdistrplus package
#' @import triangle
#' @import actuar
#' @export
#' @examples
#' \dontrun{
#'    custom_chisq(fit_best_single(rnorm(100), "triangular"))
#' }
custom_chisq <- function(custom_mle) {
  distribution <- custom_mle$distribution
  dist_fun <- match.fun(paste0('q', convert_dist(distribution)))
  data <- custom_mle$data
  param_list <- custom_mle$estimate

  # for fitdistrplus, the average bin size is 4*n^{2/5}
  # thus, the corresponding bin count in 1/4*n^{3/5}
  bin_count <- round(1/4*length(data)^{3/5})
  # create equally separated bin bounds
  quantile_numbers <- seq(from = 0, to = bin_count, by = 1)
  # convert bin bounds to [0,1]
  quantiles <- quantile_numbers/bin_count
  # take inverse CDF of bin bounds to get bin bounds of x values
  quantiles_x <- quantile_sample(distribution = distribution, quantile_vector = quantiles, param_list = param_list)
  # have to get minimum inside of cuts
  quantiles_x[1] <- quantiles_x[1] - .000001
  # cut data by bin bounds to get observed values
  obs <- cut(data, quantiles_x)
  # values have been taken to expect equal number of elements in each bin
  expected <- rep(length(data)/bin_count, bin_count)
  #If NAs given in exp, extend exp to appropriate length
  if (length(expected) < length(summary(obs))) {
    exp <- c(expected, rep(0, length(summary(obs))-length(expected)))
  }
  return (sum((summary(obs)-expected)^2/expected))
}

#' Get chisq value from MLE for custom chisq function for discrete dists
#'
#'
#'
#' @param custom_mle named list containing 'data' vector, 'distribution' name,
#'   and 'param_list' list
#' @return chisq statistic with number of bins 1/4*length(data)^{3/5} -
#'   corresponding to average bin size used in fitdistrplus package
#' @export
custom_chisq_discrete <- function(custom_mle) {
  distribution <- custom_mle$distribution
  dist_fun <- match.fun(paste0('d', convert_dist(distribution)))
  data <- custom_mle$data
  param_list <- custom_mle$estimate
  expected <- length(data)*mapply(FUN = dist_fun, x = seq(min(data), max(data)), MoreArgs = param_list)
  countValEq <- function(num, data) {
    length(which(data==num))
  }
  obs <- mapply(FUN = countValEq, num = seq(min(data), max(data)), MoreArgs = list(data = data))
  sum((obs-expected)^2/expected)
}