#' For specified distribution, find parameters for best-fitting distribution
#'
#' @param data data to fit distribution to
#' @param distribution name of the distribution to fit
#' @return parameter list of MLE for distribution on data
#' @export
#' @examples
#'    fit_best_single(rnorm(100), "normal")
#'    # fit_best_single(rtriangle(100), "triangle")
fit_best_single <- function(data, distribution){
  x <- list(data = data, distribution = distribution)
  class(x) <- c(class(x), distribution)
  fitdist_Alteryx(x)
}

#' Generic method for fitting distribution
#'
#' @param x object to fit
#' @param ... additional parameters to pass.
#' @export
fitdist_Alteryx <- function(x, ...){
  UseMethod('fitdist_Alteryx')
}

#' Default method for fitting distribution
#' Uses fitdistrplus::fitdist without starting values to optimize
#'
#' @import fitdistrplus
#' @export
#' @inheritParams fitdist_Alteryx
fitdist_Alteryx.default <- function(x, ...){
  fitdistrplus::fitdist(x$data, convert_dist(x$distribution), ...)
}

#' Fit pareto distribution
#'
#'
#'
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    xm - min x val for distribution
#'    alpha - shape parameter
#' @import actuar
#' @export
fitdist_Alteryx.pareto <- function(x, ...){
  x <- x$data
  xm <- min(x)
  if(xm <= 0) {
    stop("Error in pareto_mle: values must be positive to fit a pareto distribution")
  }
  alpha <- length(x)/(sum(log(x))-length(x)*log(xm))
  list(
    distribution = "pareto", 
    estimate = list(xm = xm, alpha = alpha), 
    data = x
  )
}

#' Fit triangular distribution
#'
#'
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    a - min x value
#'    b - man y value
#'    c - most likely x value
#' @import triangle
#' @export
fitdist_Alteryx.triangle <- function(x, ...){
  fitdist_Alteryx.triangular(x, ...)
}

#' Fit triangular distribution
#'
#'
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    a - min x value
#'    b - man y value
#'    c - most likely x value
#' @import triangle
#' @export
fitdist_Alteryx.triangular <- function(x, ...){
  x <- x$data
  a <- min(x)
  b <- max(x)
  c <- median(c(a, b, 3*sum(x)/length(x)-a-b))
  list(
    distribution = "triangular",
    estimate = list(a = a, b = b, c = c),
    data = x
  )
}

#' Fit binomial 
#' 
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    size
#'    prob
#' @export
fitdist_Alteryx.binom <- function(x, ...) {
  fitdist_Alteryx.binomial(x, ...)
}

#' Fit binomial distribution
#' 
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    size
#'    prob
#' @export
fitdist_Alteryx.binomial <- function(x, ...) {
  m <- mean(x$data)
  prob1 <- 1 - (var(x$data)/m)
  size <- round(m/prob1,0)
  size <- ifelse(size ==0, 1, size)
  prob <- m/size
  list(
    distribution = "binomial",
    estimate = list(size = size, prob = prob),
    data = x
  )
}

#' Fit poisson distribution 
#' 
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    lambda
#' @export
fitdist_Alteryx.poisson <- function(x, ...) {
  m <- mean(x$data)
  if(m > 0 && m < 1) {
    m <- 1
  } else {
    m <- round(m, 0)
  }
  list(
    distribution = "poisson",
    estimate = list(lambda = m),
    data = x
  )
}

#' Fit poisson distribution 
#' 
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    lambda
#' @export
fitdist_Alteryx.pois <- function(x, ...) {
  fitdist_Alteryx.poisson(x, ...)
}

#' Fit geometric distribution 
#' 
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    prob
#' @export
fitdist_Alteryx.geometric <- function(x, ...) {
  list(
    distribution = "geometric",
    estimate = list(prob = 1/(mean(x$data))),
    data = x
  )
}


#' Fit geometric distribution 
#' 
#' @inheritParams fitdist_Alteryx
#' @return parameter list with elements:
#'    prob
#' @export
fitdist_Alteryx.geom <- function(x, ...) {
  fitdist_Alteryx.geometric(x, ...)
}

#' Apply best fit function and catch potential errors
#' Necessary for impossible function fitting
#' Example: fitting lognormal on negative values
#' @param data vector to fit to
#' @param distribution distribution to fit to
#' @return fit object or NA (if can't be fit)
#' @export
try_fit_best_single <- function (data, distribution) {
  tryCatch(
    fit_best_single(data, distribution), 
    error=function(cond) {return(NA)}, 
    silent = TRUE
  )
}



#' Get info on fitting and gof
#'
#' @inheritParams try_fit_best_single
#' @return list with elements 'distribution', 'params', and 'chisq'
#' @export
fit_info <- function (data, distribution) {
  print("fit_info_dist")
  print(distribution)
  tried_fit <- try_fit_best_single(data, distribution)
  if(is.na(tried_fit)) {
    params <- NA
  } else  {
    params <- tried_fit$estimate
  }
  list(params = params, distribution = distribution, chisq = chi_sq(list(data = data, distribution = distribution, estimate = params)))
}

#' Generic non-S3 method for distribution fitting
#' 
#' @inheritParams fit_dists
#' @export
fit_dists2 <- function(data, dist_list) {
  if(class(data) == "numeric" || class(data) == "integer") {
    fit_dists.numeric(data, dist_list)
  } else if(class(data) == "data.frame") {
    fit_dists.data.frame(data, dist_list)
  } else {
    print(str(data))
    fit_dists.default(data, dist_list)
  }
}

#' Generic method for fitting over vector of distributions
#'
#' @param data vector to fit
#' @param dist_list data to fit to
#' @return best fits for distribution
#' @export
fit_dists <- function(data, dist_list) {
 # UseMethod('fit_dists')
  fit_dists2(data,dist_list)
}

#' Fit single vector across all distributions to best parameters
#'
#' @inheritParams fit_dists
#' @return distributions and best-fitting params
#' @export
fit_dists.default <- function(data, dist_list) {
  stop("Currently supported classes for data arg limited to numeric and data.frame")
}

fit_dists.numeric <- function(data, dist_list) {
  best_distrs <- mapply(FUN = fit_info, list(data), dist_list, SIMPLIFY = FALSE)
  names(best_distrs) <- dist_list
  best_distrs
}

#' Fit each column to best fitting distribution and distribution params
#'
#' @inheritParams fit_dists
#' @return list of distributions and best-fitting params
#' @export
fit_dists.data.frame <- function(data, dist_list) {
  fit_by_col <- function(vec) {
    fit_dists2(vec, dist_list)
  }
  apply(data, 2, FUN = fit_by_col)
}
