#' Scatter plotting
#'
#' @param data dataset to plot from
#' @param inputs vector of strings - x axis variables
#' @param outputs vector of strings - y axis variables
#' @return collection of scatterplots - one for each pair of input/output
#' @import ppcor
sa_scat <- function(data, inputs, outputs) {
  single_scat <- function(data, input, output) {
    plot(data[, input], data[, output],
         main = paste(input, " vs ", output, " plot", sep = ""),
         xlab = input, ylab = output
    )
  }
  data <- data[union(inputs, outputs)]
  par(mfrow = c(length(outputs), length(inputs)))
  for (j in outputs) {
    for (i in inputs) {
      single_scat(data, i, j)
    }
  }
}


#' Compute Importance Index
#'
#' @inheritParams sa_scat
#' @return matrix of importance indices - one for each pair of input/output
sa_importance <- function(data, inputs, outputs) {
  data <- data[union(inputs, outputs)]
  results <- as.data.frame(outer(inputs, outputs, FUN = Vectorize(function(input, output) {
    var(data[,input])/var(data[,output])
  })))
  names(results) <- outputs
  results <- cbind(independent_variable = inputs, results)
  return (results)
}



#' Compute Pearson's R matrix of correlation coefficients
#'
#' @inheritParams sa_scat
sa_r <- function(data, inputs, outputs) {
  results <- as.data.frame(outer(inputs, outputs, FUN=Vectorize(function(input, output) {
    cor(data[,input],data[,output])
  })))
  names(results) <- outputs
  results <- cbind(independent_variable = inputs, results)
  return (results)
}

#' Partial Correlation Coefficient
#'
#' @inheritParams sa_scat
#' @return matrices of partial correlation coefficients
sa_pr <- function(data, inputs, outputs) {
  sa_pr_single <- function (data, inputs, output) {
    data <- data[union(inputs, c(output))]
    results <- as.data.frame(pcor(data)$estimate)
    names(results) <- union(inputs, c(output))
    results <- cbind(independent_variable = names(results), results)
    return (results)
  }

  full_results <- list()
  for (output in outputs) {
    new_results <- sa_pr_single(data,inputs,output)
    full_results[[length(full_results)+1]] <- new_results
  }
  names(full_results) <- outputs
  return (full_results)
}
