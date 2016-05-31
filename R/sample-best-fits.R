#' Process a fit element created by 'fit_dists' on a data.frame
#'
#' @param fit_element element from highest list resulting from 'fit_dists' on a data.frame
#' @param type "MC" or "LH" for Monte Carlo or Latin Hypercube sampling
#' @return function accepting argument for number of samples to draw which would draw samples from best fitting distribution
#' @export
process_fit_element <- function (fit_element, type = "MC") {
#   print(class(fit_element))
#   print(str(fit_element))
  print(fit_element)
  print("ok1")
  #getChi2 <- function(x) x$chisq
  n <- length(fit_element)
  chi2s <- rep(NA, n)
  for (i in 1:n) {
    chi2s[i] <- fit_element[[i]]$chisq
  }
  # chi2s <- lapply(fit_element, function(x) print(class(x)), simplify = FALSE)
  # chi2s <- mapply(FUN = '[[', fit_element, "chisq")
  # chi2s <- lapply(fit_element, function(x) x$chisq, simplify = )
  print('ok2')
  best_fit_index <- which(chi2s == min(chi2s))[1]
  print(str(chi2s))
  if(chi2s[best_fit_index] == Inf) {
    stop("No successful fits")
  }
  best_fit_distribution <- fit_element[[best_fit_index]]$distribution
  best_fit_params <- fit_element[[best_fit_index]]$params
  function(m) {
    rej_sample_from_dist(distribution = best_fit_distribution, param_list = best_fit_params, type = type)(m)
  }
}

#' Apply list of functions to single argument
#'
#' @param fxns list of functions
#' @param arg arg to repeatedly apply fxns to
#' @return list of results from functions applied to args
#' @export
#' @examples
#'    fapply(c(min, max, mean), 1:5)
fapply <- function(fxns, arg) {
  mapply(do.call, fxns, MoreArgs = list(args = list(arg)))
}

#' Find best fit for each column and sample from it
#'
#' @param data dataframe to fit columns of
#' @param dist_list list of distributions whose fits to consider
#' @param type "MC" or "LH" for Monte Carlo or Latin Hypercube sampling
#' @return function that takes size argument and gives dataframe of samples
#' @export
sample_best <- function (data, dist_list = Alteryx_distributions_continuous(), type = "MC") {
  fits <- fit_dists(data = data, dist_list = dist_list)
  print(class(fits))
  # AlteryxMessage(class(fits), iType = 2, iPriority = 3)
  sampling_functions <- mapply(FUN = process_fit_element, fit_element = fits, MoreArgs = list(type = type))
  function (m) {
    as.data.frame(fapply(sampling_functions, m))
  }
}
