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
    rej_sample_from_dist(
      distribution = distribution,
      param_list = params,
      type = method,
      lower = bounds[1],
      upper = bounds[2]
    )
  )
}

#' Code for processing when process = "entire"
#'
#'
#' @param method method
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count number of samples to draw
#' @param data <insert doc>
#' @param dataName incomming data if process = "data"
#' @param replace whether or not to sample with replacement
#' @param totalSize total size
#' @return function writes to Alteryx output 1
#' @export
entire_process <- function(method, chunkSize, count, data, dataName, replace, totalSize) {
  strat <- method == 'LH'
  if(!is.null(data)) {
    doInChunks(nOutput = 1, total_size = count, chunk_size = chunkSize) (sample_df(df = data, replace = replace))
  } else {
    sampleSizes <- getSampleSizes(chunkSize, totalSize, count, replace, strat)
    mapfxn <- function(data, chunkNumber) {
      numSamples <- sampleSizes[chunkNumber]
      AlteryxRDataX::write.Alteryx(sample_df(df = data, replace = replace) (numSamples),1)
    }
    mapReduceChunkArg(dataName, chunkSize, totalSize, NULL) (mapfxn, NULL)
  }
}

#' Code for processing when process = "each"
#'
#' @inheritParams entire_process
#' @param data incomming data if process = "data"
#' @return function writes to Alteryx output 1
#' @export
each_process <- function(method, chunkSize, count, data, dataName, replace, totalSize) {
  strat <- method == 'LH'
  if(!is.null(data)) {
    doInChunks(nOutput = 1, total_size = count, chunk_size = chunkSize) (sample_df_indep(df = data, replace = replace))
  } else {
    sampleSizes <- getSampleSizes(chunkSize, totalSize, count, replace, strat)
    mapfxn <- function(data, chunkNumber) {
      numSamples <- sampleSizes[chunkNumber]
      AlteryxRDataX::write.Alteryx(sample_df_indep(df = data, replace = replace) (numSamples),1)
    }
    mapReduceChunkArg(dataName, chunkSize, totalSize, NULL) (mapfxn, NULL)
  }
}

#' Code for processing when process = "best"
#'
#' @param method sampling method used: "MC" for Monte Carlo or "LH" for Latin Hypercube
#' @param chunkSize int > 0 - maximal number of records to be processed at time
#' @param count number of samples to draw
#' @param possible vector of distribution options to be fit to data
#' @param data incomming data if process = "data"
#' @param dataName name of data input incoming
#' @return function writes to Alteryx output 1
#' @export
best_process <- function(method, chunkSize, count, data, dataName, possible) {
  if(is.null(data)) {
    data <- AlteryxRDataX::read.Alteryx(dataName)
  }
  print(str(data))
  print(dataName)
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
#' @param dataName incomming data if process = "data"
#' @param replace bool for sampling with replacement
#' @param totalSize total size
#' @return function writes to Alteryx output 1
#' @export
data_process <- function(method, chunkSize, count, process, possible, type, id, value, name, 
    roulette, dataName, replace, totalSize){
  data <- NULL
  print("process")
  print(process)
  print("type")
  print(type)
  if(type == "binned") {
    data <- AlteryxRDataX::read.Alteryx(dataName)
    idVec <- data[, id]
    valVec <- data[, value]
    data <- data.frame(id = idVec, count <- valVec)
  }
  print("data1")
  print(str(data))
  print("doing manual if")
  if(type == "manual") {
    print("doing roulette stuff")
    print(roulette)
    print(str(roulette))
    idVec <- names(roulette)
    valVec <- unlist(roulette)
    data <- data.frame(id = idVec, count <- valVec)
    print(str(data))
    type = "binned"
    print(name)
  }
  print("done manual if")
  if(type == "binned") {
    if(!is.null(id) && !is.null(value)) {
      data <- data.frame(data = bin_to_data(bins = data, idName = id, valName = value))
      print("namse")
      print(id)
      print(value)
    } else {
      data <- data.frame(data = bin_to_data(data))
    }
    names(data) <- c(name)
  }
  print(str(data))
  switch(process,
    entire = entire_process(method, chunkSize, count, data, dataName, replace, totalSize),
    each = each_process(method, chunkSize, count, data, dataName, replace, totalSize),
    best = best_process(method, chunkSize, count, data, dataName, possible)
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
#' @param process sampling method for data - "entire" to sample records, "each"
#'   to sample from columns independently, "best" to fit then sample
#' @param possible vector of distribution options to be fit to data
#' @param type whether data is "binned", comes from "roulette" widget, or is standard
#' @param id name of id field for binned data
#' @param value name of value field for binned data
#' @param name name of output vector for roulette or binned data or parametric input
#' @param roulette json string for roulette data
#' @param dataName incomming data if process = "data"
#' @param sampleSource sample source
#' @param replace whether or not to sample with replacement
#' @param totalSize total size
#' @return function writes to Alteryx output 1
#' @export
tool_process <- function(method, chunkSize, seed, count, distribution,
  params, bounds, process, possible, type, id, value, name, roulette, dataName, 
  sampleSource, replace, totalSize){
  set.seed(seed)
  switch(sampleSource,
    parametric = param_process(
      method = method,
      chunkSize = chunkSize,
      count = count,
      distribution = distribution,
      params = params,
      bounds = bounds, 
      name = name
    ),
    data = data_process(
      method = method,
      chunkSize = chunkSize,
      count = count,
      process = process,
      possible = possible,
      type = type,
      id = id,
      value = value,
      name = name,
      roulette = roulette, 
      dataName = dataName,
      replace = replace,
      totalSize = totalSize
    )
  )
}

