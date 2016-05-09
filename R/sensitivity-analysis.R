#' Function to make single scatter plot
#' 
#' @param data data for plot
#' @param independent variable name
#' @param dependent variable name
#' @return scatter plot
#' @export
singleScat <- function(data, input, output) {
  plot(
    data[,input], 
    data[,output],
    main = paste(input, " vs ", output, " plot", sep = ""),
    xlab = input,
    ylab = output
  )
}

#' Function to make collection of scatter plots
#' 
#' @param data data for plot
#' @param inputs vector of independent variables
#' @param outputs vector of dependent variables
#' @return grid of scatterplots
#' @export 
saScat <- function(data, inputs, outputs) {
  data <- data[c(inputs, outputs)]
  par(mfrow=c(length(outputs),length(inputs)))
  for (j in outputs) {
    for (i in inputs) {
      singleScat(data, i, j)
    }
  }
}

#' Importance index
#' 
#' @param data dataframe to evaluate importances from
#' @param inputs vector of independent variable names
#' @param outputs vector of dependent variable names
#' @return matrix of importance metrics
#' @export
saImportance <- function(data, inputs, outputs) {
  data <- data[c(inputs, outputs)]
  results <- as.data.frame(outer(inputs, outputs, FUN=Vectorize(function(input, output) {
    var(data[,input])/var(data[,output])
  })))
  names(results) <- outputs
  results <- cbind(independent_variable = inputs, results)
}

#' Pearson's R
#' 
#' @param data dataframe to evaluate importances from
#' @param inputs vector of independent variable names
#' @param outputs vector of dependent variable names
#' @return matrix of correlation coefficients
#' @export
saR <- function(data, inputs, outputs) {
  results <- as.data.frame(outer(inputs, outputs, FUN=Vectorize(function(input, output) {
    cor(data[,input],data[,output])
  })))
  names(results) <- outputs
  results <- cbind(independent_variable = inputs, results)
  return (results)
}

#' Partial Correlation Coefficients
#' 
#' @param data dataframe of data
#' @param inputs vector of names for independent variable
#' @param output single dependent variable name
#' @return matrix of partial correlations
#' @import ppcor
#' @export
saPrSingle <- function (data, inputs, output) {
  data <- data[union(inputs, c(output))]
  results <- as.data.frame(pcor(data)$estimate)
  names(results) <- union(inputs, c(output))
  results <- cbind(independent_variable = names(results), results)
  return (results)
}

#' Partial Correlation Coefficients
#' 
#' @param data dataframe of data
#' @param inputs vector of names for independent variable
#' @param outputs vector of dependent variable names
#' @return list of matrices of partial correlations
#' @export
saPr2 <- function(data) {
  results <- as.data.frame(pcor(data)$estimate)
  names(results) <- names(data)
  results <- cbind(Variable = names(results), results)
  results
}