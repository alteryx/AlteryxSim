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
  x <- params
  class(x) <- distribution
  errorCheckParams(x)
  AlteryxRDataX::write.Alteryx(data.frame(`temp` = 0), 1)
  bounds <- c(min(bounds), max(bounds))
  y <- bounds 
  class(y) <- distribution
  #errorCheckBounds(y)
  doInChunks(nOutput = 3, total_size = count, chunk_size = chunkSize, names = c(name)) (
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
  print("Entire process...")
  if(!is.null(data)) {
    print("Do in chunks...")
    doInChunks(nOutput = getOption('ayxsim.data.noutput'), total_size = count, chunk_size = chunkSize) (sample_df(df = data, replace = replace))
  } else {
    print("Map reduce chunk arg...")
    sampleSizes <- getSampleSizes(chunkSize, totalSize, count, replace, strat)
    mapfxn <- function(data, chunkNumber) {
      print(chunkNumber)
      print(str(data))
      numSamples <- sampleSizes[chunkNumber]
      AlteryxRDataX::write.Alteryx(
        sample_df(df = data, replace = replace) (numSamples),
        getOption('ayxsim.data.noutput')
      )
      print("Done with chunk...")
      print(chunkNumber)
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
    doInChunks(nOutput = getOption('ayxsim.data.noutput'), total_size = count, chunk_size = chunkSize) (sample_df_indep(df = data, replace = replace))
  } else {
    sampleSizes <- getSampleSizes(chunkSize, totalSize, count, replace, strat)
    mapfxn <- function(data, chunkNumber) {
      print("Processing chunk...")
      print(chunkNumber)
      numSamples <- sampleSizes[chunkNumber]
      AlteryxRDataX::write.Alteryx(
        sample_df_indep(df = data, replace = replace) (numSamples),
        getOption('ayxsim.data.noutput')
      )
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
  doInChunks(nOutput =  getOption('ayxsim.data.noutput'), 
     total_size = count, chunk_size = chunkSize) (
    sample_best(data = data, dist_list = possible, type = method)
  )
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
  print("Data Process...")
  options('ayxsim.data.noutput' = 1)
  if(type == "binned") {
    data <- AlteryxRDataX::read.Alteryx(dataName)
    idVec <- data[, id]
    valVec <- data[, value]
    data <- data.frame(id = idVec, count = valVec)
    options('ayxsim.data.noutput' = 3)
  }
  if(type == "manual") {
    idVec <- as.numeric(names(roulette))
    valVec <- unlist(roulette, use.names = F)
    data <- data.frame(id = idVec, count = valVec)
    type = "binned"
    options('ayxsim.data.noutput' = 3)
  }
  if(type == "binned") {
    data <- data.frame(data = bin_to_data(bins = data))
    names(data) <- c(name)
  }
  if (type == "raw"){
    AlteryxRDataX::write.Alteryx(data.frame(Variable = 0), 3)
  } else {
    AlteryxRDataX::write.Alteryx(data.frame(`temp` = 0), 1)
  }
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
  print("Tool Process...")
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

