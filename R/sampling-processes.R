#' Code for processing parametric inputs
#'
#' @param method sampling method used: "MC" for Monte Carlo or "LH" for Latin Hypercube
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count number of samples to draw
#' @param distribution valid distribution name
#' @param params parameter list for distribution
#' @param bounds vector - first element is lower bound, second is upper. Closed intervals only
#' @param name name of output vector for roulette or binned data or parametric input
#' @return function writes to Alteryx output 1
#' @export
param_process <- function(method, chunkSize, count, distribution, params, bounds, name)
{
  doInChunks(nOutput = 1, total_size = count, chunk_size = chunkSize, names = c(name)) (
    rej_sample_from_dist(distribution = distribution,
                         param_list = params,
                         type = method,
                         lower = bounds[1],
                         upper = bounds[2]
    )
  )
}

#' Code for processing when process = "entire"
#'
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count number of samples to draw
#' @param data incomming data if process = "data"
#' @return function writes to Alteryx output 1
#' @export
entire_process <- function(chunkSize, count, data) {
  doInChunks(nOutput = 1, total_size = count, chunk_size = chunkSize) (sample_df(df = data))
}

#' Code for processing when process = "each"
#'
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count number of samples to draw
#' @param data incomming data if process = "data"
#' @return function writes to Alteryx output 1
#' @export
each_process <- function(chunkSize, count, data) {
  doInChunks(nOutput = 1, total_size = count, chunk_size = chunkSize) (sample_df_indep(df = data))
}

#' Code for processing when process = "best"
#'
#' @param method sampling method used: "MC" for Monte Carlo or "LH" for Latin Hypercube
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count number of samples to draw
#' @param possible vector of distribution options to be fit to data
#' @param type whether data is "binned", comes from "roulette" widget, or is standard
#' @param id name of id field for binned data
#' @param value name of value field for binned data
#' @param name name of output vector for roulette or binned data or parametric input
#' @param roulette json string for roulette data
#' @param data incomming data if process = "data"
#' @return function writes to Alteryx output 1
#' @export
best_process <- function(method, chunkSize, count, possible, type, id, value, name, roulette, data) {
  doInChunks(nOutput = 1, total_size = count, chunk_size = chunkSize) (sample_best(data = data, dist_list = possible, type = method))
}

#' Code for processing data inputs
#'
#' @param method sampling method used: "MC" for Monte Carlo or "LH" for Latin Hypercube
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count count
#' @param process sampling method for data - "entire" to sample records, "each" to sample from columns independently, "best" to fit then sample
#' @param possible vector of distribution options to be fit to data
#' @param type whether data is "binned", comes from "roulette" widget, or is standard
#' @param id name of id field for binned data
#' @param value name of value field for binned data
#' @param name name of output vector for roulette or binned data or parametric input
#' @param roulette json string for roulette data
#' @param data incomming data if process = "data"
#' @return function writes to Alteryx output 1
#' @export
data_process <- function(method, chunkSize, count, process, possible, type, id, value, name, roulette, data)
{
  if(type == "roulette") {
    data <- string_to_bin(roulette)
    type = "binned"
  }
  if(type == "binned") {
    data <- bin_to_data(data)
  }
  switch(process,
         entire = entire_process(chunkSize, count, data),
         each = each_process(chunkSize, count, data),
         best = best_process(method, chunkSize, count, process, possible, type, id, value, name, roulette)
  )
}

#' Code for sampling tool in Alteryx
#'
#' @param method sampling method used: "MC" for Monte Carlo or "LH" for Latin Hypercube
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param seed seed to intialize tool with
#' @param count number of samples to draw
#' @param distribution valid distribution name
#' @param params parameter list for distribution
#' @param bounds vector - first element is lower bound, second is upper. Closed intervals only
#' @param process sampling method for data - "entire" to sample records, "each" to sample from columns independently, "best" to fit then sample
#' @param possible vector of distribution options to be fit to data
#' @param type whether data is "binned", comes from "roulette" widget, or is standard
#' @param id name of id field for binned data
#' @param value name of value field for binned data
#' @param name name of output vector for roulette or binned data or parametric input
#' @param roulette json string for roulette data
#' @param data incomming data if process = "data"
#' @param sampleSource sample source
#' @return function writes to Alteryx output 1
#' @export
tool_process <- function(method, chunkSize, seed, count, distribution,
                         params, bounds, process, possible, type,
                         id, value, name, roulette, data, sampleSource = 'parametric'
                         )
{
  set.seed(seed)

  switch(sampleSource,
         parametric = param_process(method = method,
                                    chunkSize = chunkSize,
                                    count = count,
                                    distribution = distribution,
                                    params = params,
                                    bounds = bounds),

         data = data_process(method = method,
                             chunkSize = chunkSize,
                             count = count,
                             process = process,
                             possible = possible,
                             type = type,
                             id = id,
                             value = value,
                             name = name,
                             roulette = roulette)
  )

}
