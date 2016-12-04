# autoCaret is a package for automating Classification and Regression training
#
# it's functionality relies heavily on those packages that precede it
# namely caret (Max Kuhn) & caretEnsemble (Zach Mayer)
#
#
# Authors: Greg Ceccarelli, Michael Marks, Rock Baek
# 2016-11-26
###############################################################################

#'  Function to wrap and automate datasplitting into train and test sets
#'
#' @name autoTrainTestSplit
#' @param df The dataframe to split into training and test
#' @param y The response variable in thataset
#' @param set_seed The seed to use to ensure that examples are reproducible
#' @param side_effects Whether or not to write training and test dataframe to the global env.
#' @param p The percentage of data that goes to training
#' @export
#' @examples
#'
#' train_test_split(stroke.death.predicition, Y)
#'

autoTrainTestSplit <- function(df, y, set_seed = 1234, side_effects = FALSE, p=.8, ...) {
  suppressMessages(attach(df))
  # get access to the target variable
  # y <- with(df, y)

  # initialize state to not be random
  set.seed(set_seed)

  # create a train index using initalized parameters parameters
  trainindex <- caret::createDataPartition(y, p = p,  list = FALSE, times = 1, ...)

  # stores names of training & test set in variables
  train <- paste(deparse(substitute(df)), "Train", sep="")
  test <- paste(deparse(substitute(df)), "Test", sep="")

  if (side_effects == TRUE) {
  # return train and test split dfs to Global env
  assign(train, df[trainindex, ], envir = .GlobalEnv)
  assign(test, df[-trainindex, ], envir = .GlobalEnv)
  }
    else {
      detach(df)
      return(list(
        train = df[trainindex, ]
        , test = df[-trainindex, ]
        ))
    }
}

#'  Wrapper function to allow for "1 Click Modeling"
#'
#' @name autoModel
#' @param df The dataframe to use for binary predictions
#' @param y The response variable in the dataset
#' @details The goal of this function is to allow for simple one click modeling of an example dataset.
#' The dataset should be of moderate size (nrow(df) < 1e6) and have a binary response variable (e.g. length(levels(df$y))==2). If a continuous variable is
#' passed in as the response parameter, the function will attempt to binarize it based on a distribution summarization function (median, mean, etc.)
#' Additionally, this function will automatically attempt to detect class imbalance and correct the input training dataset for the user.
#' Currently, this function does not support multiclass classification problems.
#' @param method_list Allows the user to pass in a list of models to use to make their ensemble, if NULL, the default is:
#' @param progressBar Allows the user to toggle verbose output about function execution (show a progress bar with other detail)
#' @param subsample Allows the user to pass in a \code{caret} subsampling function. Default = downSample.
#' @return an \code{\link{autoCaret}} object
#' @export
#' @examples
#'
#' oneClick <- autoModel(Sonar, Class)
#'
#'

## To Do
# 1. Code to check for class imbalance (DONE)
# 2. Code to handle training rf, etc when there are a huge number of predictors
# 3. Code optimize hyperparameter search (ON-HOLD)
# 4. Check if ensemble would produce improved performance over individual models
# 6. Generic extensions to handle printing out info about autoModel
# 7. Parallel processing to improve performance
# 8. Code to generate an output Rmarkdown document with all information about process


autoModel <- function(df, y, method_list=NULL, progressBar=TRUE, subsample = downSample, pp_method_list = c("center", "scale")) {
  ########################################
  ## Section 1: Input Validation ##
  ########################################
  suppressMessages(require(magrittr))
  suppressMessages(require(caret))
  suppressMessages(require(scales))

  #define a list to store step results
  autoModelList <- list()

  #define a sublist to store steps record
  autoModelList$steps_conducted <- list()

  #second validate that input is a data frame, if not stop
  stopMessage(exists(deparse(substitute(df))), "Object doesn't exist!")

  #second validate that input is a data frame, if not stop
  stopMessage(is.data.frame(df), "Input is not a dataframe and is not suitable for automatic prediction")

  #attach the dataframe
  suppressMessages(attach(df))

  #store input y variable name
  autoModelList$y_name <- deparse(substitute(y))

  # start the clock
  ptm <- proc.time()

  # Create progress bar
  invisible(capture.output(PB <- txtProgressBar(min = 0, max = 20, style = 3, char = " ")))
  flush.console()

  #second validate that y exists and is binary. If it is isn't, attempt to coerce it
  if (missing(y)) {
    stop("Please specify your prediction target")
  } else {
    # instead of attach, use with to access the variable from the df
    # target <- with(df, y)
    target <- y
    # remove original target and store copy as vector
    df[[deparse(substitute(y))]] <- NULL
    df$y <- NULL

    if (checkBinaryTrait(target)) {

      #if is binary, then just return target as target
      target <- coerceToBinary(target)

      autoModelList$steps_conducted$target_coerced <- FALSE
      autoModelList$y <- target
      autoModelList$y_old <- target
    } else {
      autoModelList$y_old <- target

      target <- coerceToBinary(target)

      if (is.logical(target) && target[1] == FALSE) stop("Non coercible targets aren't accepted")
      autoModelList$steps_conducted$target_coerced <- TRUE
      autoModelList$y <- target
    }
  }


  if (autoModelList$steps_conducted$target_coerced) {
    lvls <- paste(levels(unique(autoModelList$y)),collapse = " & ")
    progressWrapper(paste("Input validation step complete & target variable was binarized to ", lvls, sep = ""), time=2, pb=PB, verbose = progressBar)
  } else {
    progressWrapper("Validation step complete", time=2, pb=PB, verbose = progressBar)
  }

  ## make sure the binarized target variable is on the dataframe
  df$y <- target
  autoModelList$df <- df


  ########################################
  ## Section 3: Preprocessing ##
  ########################################
  #save pp method
  autoModelList$pp_method_list <- pp_method_list
  #Near Zero Variance
  autoModelList$WaysToImprove$nzv$cols <- caret::nearZeroVar(autoModelList$df)
  if(length(autoModelList$WaysToImprove$nzv$cols) > 0) {
    autoModelList$WaysToImprove$nzv$names <- colnames(autoModelList$df)[autoModelList$WaysToImprove$nzv$cols]
    autoModelList$WaysToImprove$nzv$flag <- TRUE
    } else {
    autoModelList$WaysToImprove$nzv$flag <- FALSE
    autoModelList$WaysToImprove$nzv$names <- NA
  }

  #select only numeric fields
  nums <- sapply(autoModelList$df, is.numeric)
  tempdf <- autoModelList$df[ , nums]

  #Highly Correlated Fields

  autoModelList$WaysToImprove$HighCor$cols <- caret::findCorrelation(cor(tempdf), cutoff = .75)
  if(length(autoModelList$WaysToImprove$HighCor$cols) > 0) {
    autoModelList$WaysToImprove$HighCor$names <- colnames(tempdf)[autoModelList$WaysToImprove$HighCor$cols]
    autoModelList$WaysToImprove$HighCor$flag <- TRUE
  } else {
    autoModelList$WaysToImprove$HighCor$names <- NA
    autoModelList$WaysToImprove$HighCor$flag <- FALSE
  }

  #Linear Dependency
  autoModelList$WaysToImprove$LinearDep$cols <- caret::findLinearCombos(tempdf)$remove
  if(!is.null(autoModelList$WaysToImprove$LinearDep$cols)) {
    autoModelList$WaysToImprove$LinearDep$names <- colnames(tempdf)[autoModelList$WaysToImprove$LinearDep$cols]
    autoModelList$WaysToImprove$LinearDep$flag <- TRUE
  } else {
    autoModelList$WaysToImprove$LinearDep$flag <- FALSE
    autoModelList$WaysToImprove$LinearDep$names <- NA
  }

  autoModelList$df_processed <- preprocessDummifyCenterScaleNzv(autoModelList$df, y, pp_method_list = pp_method_list)

  progressWrapper("Data effectively preprocessed", time=4, pb=PB, verbose = progressBar)


  ########################################
  ## Section 3: Data Splitting ##
  ########################################

  autoModelList$data <- autoTrainTestSplit(autoModelList$df_processed, y)

  progressWrapper("Data split into train & test", time=8, pb=PB, verbose = progressBar)

  ########################################
  ## Section 4: Model Fitting ##
  ########################################


  ########################################
  ## logic to handle class imbalances
  ########################################
  classImbalance <- checkClassImbalance(autoModelList$data$train$y)

  if (classImbalance$imbalanced) {
    progressWrapper(paste("Data subsampled to deal with class imbalance: ", classImbalance$fraction), time=10, pb=PB, verbose = progressBar)
    sub_train <- subsample(x = autoModelList$data$train[, -ncol(autoModelList$data$train)],
               y = autoModelList$data$train$y, yname="y")
    autoModelList$data$train_orig <- autoModelList$data$train
    autoModelList$data$train <- sub_train
    autoModelList$steps_conducted$data_subsampled <- TRUE
  } else {
    progressWrapper(paste("No class imbalance detected:  ", classImbalance$fraction), time=10, pb=PB, verbose = progressBar)
    autoModelList$steps_conducted$data_subsampled <- FALSE

  }

  ########################################
  ## need to add more logic here to optimize hyperparameter search
  ########################################

  #dynamically change resampling count depending on dataset size
  example_sizes <- c(sqrt(10000), sqrt(25000), sqrt(50000))
  size_d <- sqrt(nrow(autoModelList$data$train))
  r <- rescale(c(size_d,example_sizes), to = c(10,5))
  autoModelList$number_resamples <- floor(r[1])

  my_control <- caret::trainControl(
    method="boot",
    number=autoModelList$number_resamples,
    savePredictions="final",
    classProbs=TRUE,
    index=caret::createResample(autoModelList$data$train$y, autoModelList$number_resamples),
    summaryFunction=caret::twoClassSummary
  )


  if (is.null(method_list)) autoModelList$method_list <- c("glm", "rpart","rf","xgbLinear") else autoModelList$method_list <- method_list

  autoModelList$model_list <- suppressMessages(suppressWarnings(caretEnsemble::caretList(
    y ~., data=autoModelList$data$train,
    trControl=my_control,
    methodList=autoModelList$method_list
  )))

  progressWrapper("Models trained", time=12, pb=PB, verbose = progressBar)


  ########################################
  ## Add code to acutally determine if an ensemble is necessary
  ########################################

  autoModelList$ensemble_model <- suppressWarnings(caretEnsemble::caretEnsemble(
    autoModelList$model_list, # right now an ensemble is generated regardless
    metric="ROC",
    trControl=caret::trainControl(
      number=2,
      summaryFunction=caret::twoClassSummary,
      classProbs=TRUE
    )))


  progressWrapper("Models blended", time=16, pb=PB, verbose = progressBar)

  ########################################
  ## Section 6:  Perf & Model Fitting
  ########################################

  #store variable importance
  autoModelList$variable_importance <- as.data.frame(caret::varImp(autoModelList$ensemble_model)) %>%
    dplyr::mutate(variable = row.names.data.frame(.)) %>%
    dplyr::arrange(desc(overall))

  autoModelList$test_y <- autoModelList$data$test$y
  autoModelList$data$test$y <- NULL

  #use the ensemble model + test set to make predictions
  autoModelList$predictions <- predict(autoModelList$ensemble_model, newdata=autoModelList$data$test)

  #since we only are allowing binary classification right now, output a confusionMatrix
  #some issues with ensemble models: https://github.com/zachmayer/caretEnsemble/pull/190
  autoModelList$confusionMatrix <- caret::confusionMatrix(data = autoModelList$predictions, reference = autoModelList$test_y)

  progressWrapper("Predictions and Performance Metrics Generated", time=20, pb=PB, verbose = progressBar)

  autoModelList$elapsed_time <- proc.time() - ptm

  ## eventually figure out why with(...) isn't working sufficiently
  #suppressMessages(detach(deparse(substitute(df))))
  class(autoModelList) <- "autoCaret"
  return(invisible(autoModelList))
}


#' @title Check if an object is a autoCaret object
#' @param object an R object
#' @description Check if an object is a autoCaret object
#' @export
is.autoCaret <- function(object){
  is(object, "autoCaret")
}

#' @title Summarize the results of an autoCaret object for the user
#' @description Summarize an autoCaret object
#' @param object a \code{\link{autoCaret}}
#' @param ... optional additional parameters.
#' @export
#' @examples
#' \dontrun{
#' }
#'
summary.autoCaret <- function(object, ...) {
  ans <- list()
  ans$input_row_count <- nrow(object$df)
  ans$input_col_count <- ncol(object$df)
  ans$train_row_count <- nrow(object$data$train)
  ans$test_row_count <-  nrow(object$data$test)
  ans$modeling_time <- round(as.vector(object$elapsed_time[3] / 60),2)
  ans$number_resamples <- object$number_resamples
  ans$method_list <- object$method_list
  ans$confusionMatrix <- object$confusionMatrix$table
  ans$accuracy <- as.vector(round((object$confusionMatrix$overall[1])*100,2))
  ans$sensitivity <- as.vector(round((object$confusionMatrix$byClass[1])*100,2))
  ans$specificity <- as.vector(round((object$confusionMatrix$byClass[2])*100,2))
  ans$precision <- as.vector(round((object$confusionMatrix$byClass[5])*100,2))
  ans$recall <-  as.vector(round((object$confusionMatrix$byClass[6])*100,2))

  Get_Model_Summaries <- function(autoModelList){
    models_used <- names(autoModelList$model_list)
    #create function to extract the results from the iteration with the best ROC for a given model name.
    Return_Best_Model_Results <- function(model_name){
      results_string <- paste("autoModelList$model_list$",model_name,"$results")
      results <- eval(parse(text = results_string))
      max_ROC <- max(results$ROC)
      #return the row with the mox ROC. Take only first row in case there are ties and only return the columns consistant between model types.
      best_model_results <- results[results$ROC == max_ROC,][1,][c('ROC','Sens','Spec', 'ROCSD','SensSD','SpecSD')]
      cbind(model_name,best_model_results)
    }
    #return the mest model results for each model used.
    summary_list <- lapply(models_used,Return_Best_Model_Results)
    overall_summary <- do.call("rbind",summary_list)
    overall_summary <- overall_summary[order(overall_summary$ROC,decreasing = TRUE),]
    ensemble_summary <- cbind("ensemble",autoModelList$ensemble_model$ens_model$results[c('ROC','Sens','Spec', 'ROCSD','SensSD','SpecSD')])
    names(ensemble_summary)[1] <- "model_name"
    overall_summary <- rbind(ensemble_summary,overall_summary)
    rownames(overall_summary) <- seq(1,nrow(overall_summary))
    overall_summary
  }
  ans$best_model_results <- Get_Model_Summaries(object)

  cat(paste0("The input dataset had: ", ans$input_row_count, " observations and ", ans$input_col_count-1 ," predictors \n"))
  cat("---------------------\n")
  cat("Prior to model training, the input dataset was split into a training & test set \n")
  cat("The training set has: ", ans$train_row_count, " observations \n")
  cat("The test set has:     ", ans$test_row_count, " observations \n")
  stepNames <- names(object$steps_conducted)
  if (any(unlist(object$steps_conducted))) {
  cat("---------------------\n")
  cat("In addition to standard pre-processing, prior to building the model, the following things were conducted: \n")
    for (x in 1:length(object$steps_conducted)) {
      if (object$steps_conducted[x] == TRUE){
        cat(paste0(stepNames[x], "\n"))
      }
    }
  }
  cat("---------------------\n")
  cat(paste0("Overall modeling took: ", ans$modeling_time, " minutes \n"))
  cat(paste0("During that time the training data was boostrap resampled ", ans$number_resamples, " times \n"))
  cat("\n")
  cat(paste0("The following classification models were used to create an ensemble: \n"))
  cat(paste0("- ", ans$method_list), sep="\n")
  cat("To learn more about ensemble learing in the context of machine learning, please visit: https://en.wikipedia.org/wiki/Ensemble_learning \n")
  cat("\n")
  cat(paste0("In the ensemble, the top 5 variables in order from highest to lowest level of relative importance, were: \n"))
  cat(paste("- ", head(object$variable_importance)$variable[1:5]), sep="\n")
  cat("---------------------\n")
  cat(paste0("When the ensemble model was used to predict on the held out test set of ", ans$test_row_count, " observations it performed as follows: \n"))
  cat("\n")
  cat(paste0("Overall Accuracy: ", ans$accuracy, "\n"))
  cat("\n")
  cat("A confusion matrix demonstrating accuracy is as follows: \n")
  print(ans$confusionMatrix)
  cat("\n")
  cat(paste0("Precision: ", ans$precision, "\n"))
  cat(paste0("Recall: ", ans$recall, "\n"))
  cat("---------------------\n")
  cat("To learn more about Precision & Recall in the context of information retriveal, please visit: https://en.wikipedia.org/wiki/Precision_and_recall \n")
  class(ans) <- "summary.autoCaret"
  return(invisible(ans))
}

#' @title Extends predict functionality for autoCaret objects
#' @description Extends caret predict.train to \code{\link{autoCaret}}
#' @param object a \code{\link{autoCaret}}
#' @param ... optional additional parameters. Should correspond to caret::predict.train(object, newdata = NULL, type = "raw", na.action = na.omit)
#' @export
#' @examples
#' \dontrun{
#' }
#'
predict.autoCaret <- function(object, newdata=NULL, ...) {
  if (exists(deparse(substitute(newdata)))) {
    y_name <- as.name(object$y_name)
    newdata <- preprocessDummifyCenterScaleNzv(newdata, y_name, predict.autoCaret=TRUE, pp_method_list = object$pp_method_list)
  } else {
    stop("You must pass in new data to make predict on")
  }
 #autoCaret:::checkNewData(object$data$train, newdata)
predict(object$ensemble_model, newdata=newdata)
}

