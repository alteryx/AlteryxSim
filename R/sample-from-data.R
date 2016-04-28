#' Get sample sizes for simple (non-stratified) sampling
#' 
#' @param chunkSizes vector of chunk sizes
#' @param totalSize total data size
#' @param sampleCount number of total samples to draw
#' @param replace bool - sample with replacement
#' @return vector of sample sizes
#' @export
nonStratSampling <- function(chunkSizes, totalSize, sampleCount, replace = FALSE) {
  m <- sampleCount
  n <- totalSize - sampleCount
  k <- chunkSizes[1]
  sampleSizes <- c()
  for(i in 1:length(chunkSizes)) {
    size <- ifelse(replace,
                   rbinom(1, k, m/(n+m)),
                   rhyper(1, m, n, k)
    )
    sampleSizes <- c(sampleSizes, size)
    m <- m - size
    n <- n - (k - size)
    k <- chunkSizes[min(i+1, length(chunkSizes))]
  }
}

#' Get sample sizes for stratified sampling
#' 
#' @param chunkSize maximal chunk size
#' @param totalSize total data size
#' @param sampleCount number of total samples to draw
#' @return vector of sample sizes
#' @export
stratSampling <- function(chunkSize, totalSize, sampleCount) {
  sampleSizes <- diff(round(c(seq(from = 0, to = sampleCount, by = sampleCount*chunkSize/totalSize), sampleCount)))
  if(sampleSizes[length(sampleSizes)]==0) {
    sampleSizes <- sampleSizes[1:(length(sampleSizes)-1)]
  }
  return(sampleSizes)
}

#' Get sample sizes for chunks
#' 
#' @param chunkSize maximal chunksize
#' @param totalSize total data size
#' @param sampleCount number of total samples to draw
#' @param replace bool - sample with replacement
#' @param strat bool - stratified sampling
getSampleSizes <- function(chunkSize, totalSize, sampleCount, replace = FALSE, strat = FALSE) {
  if(!replace && sampleCount > totalSize) {
    stop("Cannot have more samples than records when replace = FALSE")
  }
  chunkSizes <- get_chunk_sizes(chunkSize, totalSize)
  if(length(chunkSizes) == 1) {
    return(chunkSizes)
  }
  if(!strat) {
    return(nonStratSampling(chunkSizes, totalSize, sampleCount))
  } else {
    return(stratSampling(chunkSize, totalSize, sampleCount))
  }
}


#' Sample entire records from dataset
#'
#' @param df dataframe from which to sample
#' @param replace bool - whether to sample with replacement
#' @param ... additional parameters for 'sample' function
#' @return samples from dataframe
#' @export
sample_df <- function(df, replace = TRUE, ...) {
  function(m){
    df_new <- as.data.frame(df[sample(1:NROW(df), size = m, replace = replace, ...),])
    names(df_new) <- names(df)
    df_new
  }
}

#' Sample from dataset, sampling from columns independently
#'
#' @inheritParams sample_df
#' @return samples from dataframe
#' @export
sample_df_indep <- function(df, replace = TRUE, ...) {
  function(m){
    df_new <- as.data.frame(df, sample, size = m, replace = replace, ...)
    names(df_new) <- names(df)
    df_new
  }
}

