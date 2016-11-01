#' Check for missing predictors in score data as compared to the model
#' 
#' 
#' @param model model object
#' @param data data to score
checkForMissingXVars <- function(model, data){
  x.vars <- AlteryxPredictive::getXVars(model)
  data.names <- names(data)
  missing.vars <- x.vars[!(x.vars %in% data.names)]
  vars_in_msg <- paste(missing.vars, collapse = ", ")
  if (length(missing.vars) == 1) {
    stop(paste("The variable", vars_in_msg, "is missing from the input data stream."))
  } else if (length(missing.vars) > 1) {
    stop(paste("The variables",  vars_in_msg, "are missing from the input data stream."))
  } else {
    return(x.vars)
  }
}

#' Remove missing levels from score data based on model data
#' 
#' 
#' 
#' @param model model object
#' @param data data to score 
removeMissingLevels <- function(model, data){
  rel.data <- data
  xlevels <- AlteryxPredictive::getXlevels(model)
  rd.levels <- lapply(Filter(is.factor, rel.data), levels)
  mod.class <- class(model)[1]
  # Remove the records and re-do the factors to remove the missing levels
  if (!(mod.class %in% c("gbm", "rxDTree", "rxDForest"))) {
    for (i in names(xlevels)) {
      if(!all(rd.levels[[i]] %in% xlevels[[i]])) {
        extra <- rd.levels[[i]][!(rd.levels[[i]] %in% xlevels[[i]])]
        if (length(extra) == 1)
          the.message <- paste("The category", extra, "in the field", i, "was not present in the data used to estimate the model. Records with this category value will not be scored.")
        else
          the.message <- paste("The categories", paste(extra, collapse = ", "), "in the field", i, "were not present in the data used to estimate the model. Records with these category values will not be scored.")
        #AlteryxRDataX::AlteryxMessagec(the.message, iType = 2, iPriority = 3)
        print(the.message)
        rel.data <- rel.data[!(as.character(rel.data[[i]]) %in% extra), , drop = FALSE]
        new.factor <- as.factor(as.character(rel.data[[i]]))
        rel.data[[i]] <- NULL
        rel.data[[i]] <- new.factor
        rm(new.factor)
      }
    }
  }
  return(rel.data)
}

#' Score a model
#' 
#' @param model model object
#' @param data data to score
#' @param ... other parameters
prepareDataForScoring <- function(model, data){
  mod.class <- class(model)[1]
  
  # Check for missing fields and reduce data to what we need for scoring
  x.vars <- checkForMissingXVars(model, data)
  rel.data <- data[ , x.vars, drop = FALSE]
  
  # Special treatment for randomForest
  if (mod.class == "randomForest.formula") {
    rel.data <- na.omit(rel.data)
  }
  
  # Adjust score data to remove missing levels 
  rel.data <- removeMissingLevels(model, rel.data)
  cbind(asdfasdfRecordIDasdfasdf = data$asdfasdfRecordIDasdfasdf, rel.data)
}


#' Error check to ensure model class matches "isGLM"
#'
#' @param model model to check
#' @param isLM boolean to compare
#' @return no return - just throws error if mismatch
#' @export
scoreErrorCheck <- function(model, isLM) {
  lmClass <- class(model)=="lm"
  if(isLM && !lmClass) {
    stop("Model is not lm. Connect a validation dataset.")
  }
  if(!isLM && lmClass) {
    stop("Model is lm. Disconnect validation dataset.")
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

#' Function to get error vector
#' 
#' @param the.data data 
#' @param mod.obj model object
#' @return vector of errors
#' @export
getErrorVec <- function(the.data, mod.obj) {
  tarVarName <- getTarVarName(mod.obj)
  validation <- prepareDataForScoring(mod.obj, the.data)
  predVal <- AlteryxPredictive::scoreModel(mod.obj, validation[,-1])
  if(NCOL(predVal)>2) {
    stop("Scoring on categorical variables with more than 2 levels not supported")
  }
  actualVal <- convertTo01(the.data[rownames(predVal), tarVarName])
  predVal <- predVal[,NCOL(predVal)]
  actualVal - predVal
}

#' Helper function for simulateNonGLM for scores < 0 or > 1
#'
#' @param x value to fix score of
#' @return proper score
#' @export
getScore <- function(x) {
  median(c(0,x,1))
}

#' function to draw from error distribution and add to predictions (assumes homoscedasticity)
#' 
#' @param mod.obj model object
#' @param errors error vector to draw from
#' @param nsim number of draws per record
#' @return function taking in new data and giving results
#' @export
simNonGLM <- function(mod.obj, errors, nsim) {
  function(scoreData){
    RecordID <- scoreData[,1]
    scoreData <- prepareDataForScoring(mod.obj, scoreData)
    # we dont need to remove the recordID column
    scores <- AlteryxPredictive::scoreModel(mod.obj, scoreData[,-1])
    scores <- scores[,NCOL(scores)]
    
    errorSample <- sample(errors, length(scores)*nsim, replace = TRUE)
    
    results <- errorSample + scores
    new.data <- AlteryxPredictive::matchLevels(
      scoreData, 
      AlteryxPredictive::getXlevels(mod.obj)
    )
    y.levels <- AlteryxPredictive::getYlevels(mod.obj, new.data)
    if (!is.null(y.levels)){
      results <- sapply(results, getScore)
    }
    scoreData <- scoreData[rep(seq_len(nrow(scoreData)), nsim), ]
    scoreData$"asdfasdfresultsasdfasdf" <- results
    scoreData$"asdfasdfRecordIDasdfasdf" <- RecordID
    scoreData[,c('asdfasdfresultsasdfasdf', 'asdfasdfRecordIDasdfasdf')]
  }
}

#' Repurpose {stats} package 'simulate' function to work on new data
#' 
#' 
#' @param mod.obj model object to score from
#' @param nsim number of draws per record
#' @return function taking new data and simulating scores
#' @export
simGLM <- function(mod.obj, nsim) {
  function(scoreData) {
    RecordID <- scoreData[,1]
    scoreData <- prepareDataForScoring(mod.obj, scoreData)
    preds <- AlteryxPredictive::scoreModel(mod.obj, scoreData)
    scores <- preds[,NCOL(preds)]
    names(scores) <- sapply(1:length(scores), toString)
    mod.obj$fitted.values <- scores
    x <- as.vector(unlist(simulate(object = mod.obj, nsim = nsim, seed = NULL, type = "link")))
    data <- scoreData[rep(seq_len(nrow(scoreData)), nsim), ]
    data$"asdfasdfresultsasdfasdf" <- x
    data$"asdfasdfRecordIDasdfasdf" <- RecordID
    data[,c('asdfasdfresultsasdfasdf', 'asdfasdfRecordIDasdfasdf')]
  }
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

#' convert character vector to 0/1
#' 
#' @param vec vector to convert
#' @return vector of 0/1
#' @export
convertTo01.factor <- function(vec) {
  as.numeric(vec)-1
}

#' Convert default vector to 0/1
#'
#' @param vec vector to convert
#' @return vector of 0/1
#' @export
convertTo01.default <- function(vec) {
  vec
}

#' Load any needed packages based on the class of the model
#' 
#' @param model model to score
loadPackageForScoring <- function(model){
  pkgs <- c(
    earth = "earth", rpart = "rpart", randomForest.formula = "randomForest",
    svyglm = "survey", negbin = "MASS", gbm = "gbm", naiveBayes = "e1071", 
    svm.formula = "e1071", nnet.formula = "nnet", coxph = "survival"
  )
  pkgToLoad <- unname(pkgs[sapply(names(pkgs), inherits, x  = model)])
  print(paste0("library ", pkgToLoad, " is loading"))
  if(length(pkgToLoad) > 0) { 
    library(pkgToLoad, character.only = TRUE)
  }
}

#' Function to score model with added samples from an error distribution
#' 
#' @param model model object to score against
#' @param isGLM boolean of whether model is of class lm/glm
#' @param mult number of samples from error distribution per record
#' @param totalCountSim total number of incoming records
#' @param validationData dataframe of validation data
#' @param seed random seed
#' @param chunkSize maximal size of incoming data chunk
#' @return function taking data and scoring on it, sampling from error dist
#' @export
scoreProcess <- function(model, isGLM, mult, totalCountSim, validationData, seed, chunkSize) {
  loadPackageForScoring(model)
  scoreErrorCheck(model, isGLM)
  set.seed(seed)
  tarVarName <- getTarVarName(model)
  if(isGLM) {
    mapfxn <- function(x) {
      simData <- simGLM(model, mult) (x)
      AlteryxRDataX::write.Alteryx(simData, 1)
    }
  } else {
    errors <- getErrorVec(validationData, model)
    mapfxn <- function(x) {
      simData <- simNonGLM(model,errors, mult) (x)
      AlteryxRDataX::write.Alteryx(simData, 1)
    }
  }
  mapReduce("sim", chunkSize, totalCountSim, NULL) (mapfxn, NULL)
}