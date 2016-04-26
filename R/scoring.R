#' Repurpose {stats} package 'simulate' function to work on new data
#'
#' @param object model object to score from
#' @param nsim number of draws per record
#' @return function taking new data and simulating scores
#' @export
simulateGLM <- function(object, nsim) {
  function(new_data) {
    new_preds <- predict(object, new_data)
    object$fitted.values <- new_preds
    simulate(object = object, nsim = nsim, seed = NULL)
  }
}

#' Code to execute core of simulation scoring code
#'
#'
#' @export
#' @param model model object to score from
#' @param isGLM boolean predicting whether object will be glm/lm class
#' @param mult number of draws from error distribution for each record
#' @param totalCountSim total number of records to be scored
#' @param validation validation data incoming
#' @param seed random seed
#' @param scoreName name to give to scored variable
#' @param chunkSize maximal chunk size for scored data
#' @return nothing - function writes out score df to Alteryx
scoreProcess <- function(model, isGLM, mult, totalCountSim, validation, seed, chunkSize,
    scoreName) {
  scoreErrorCheck(model, isGLM)
  set.seed(seed)
  tarVarName <- getTarVarName(model)
  mapfxn <- function(x) {
    if(isGLM) {
      simData <- unlist(simulateGLM(model, mult)(x))
    } else {
      predVal <- as.vector(predict(model, validation))
      actualVal <- as.vector(validation[,tarVarName])
      errors <- actualVal - predVal
      simData <- unlist(simulateNonGLM(model, mult, errors) (x))
    }
    df <- data.frame(score = simData)
    names(df) <- scoreName
    AlteryxRDataX::write.Alteryx(df, 1)
  }
  mapReduce("sim", chunkSize, totalCountSim, NULL) (mapfxn, NULL)
}


#' Error check to ensure model class matches "isGLM"
#'
#' @param model model to check
#' @param isGLM boolean to compare
#' @return no return - just throws error if mismatch
scoreErrorCheck <- function(model, isGLM) {
  glmClass <- any(class(model)=="glm") || any(class(model)=="lm")
  if(isGLM && !glmClass) {
    stop("Model is not glm or lm. Connect a validation dataset.")
  }
  if(!isGLM && glmClass) {
    stop("Model is glm or lm. Disconnect validation dataset.")
  }
}

#' Extract target variable name from model object
#'
#' @param model model from which to extract target variable
#' @return string - name of model's target variable
#' @export
getTarVarName<- function(model){
  as.character(model$terms[[2]])
}

#' Simulation function assuming homoscedasticity
#'
#' @param object model object to score from
#' @param nsim number of draws per record
#' @param errorVec error vectorf
#' @return function taking new data and simulating scores
#' @export
simulateNonGLM <- function(object, nsim, errorVec) {
  function(newdata) {
    fullErrors <- sample(errorVec, length(newdata)*nsim, replace = TRUE)
    newPreds <- as.vector(predict(object, newdata = newdata))
    rep(newPreds, nsim) + fullErrors
  }
}

#' Helper function for simulateNonGLM for scores < 0 or > 1
#'
#' @param x value to fix score of
#' @return proper score
getScore <- function(x) {
  median(c(0,x,1))
}

#' Generic method for converting vector to 0/1
#'
#' @param vec vector to convert
#' @return vector of 0/1
#' @export
convertTo01 <- function(vec) {
  UseMethod('convertTo01')
}


#' convert character of Yes/No vector to 0/1
#'
#' @param vec vector to convert
#' @return vector of 0/1
#' @import stringr
#' @export
convertTo01.character <- function(vec) {
  vec <- tolower(str_trim(vec))
  if(!any(vec == "yes") || !any(vec =="no")) {
    stop("must be yes/no")
  }
  ifelse(vec=="yes", 1, 0)
}

#' Convert default vector to 0/1
#'
#' @param vec vector to convert
#' @return vector of 0/1
#' @export
convertTo01.default <- function(vec) {
  vec
}
