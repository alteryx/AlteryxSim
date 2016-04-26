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

