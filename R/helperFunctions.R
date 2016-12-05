#####################################################
# Misc. Functions
#####################################################

stopMessage <- function(condition, message) {
  if (missing(message)) message <- "Validation Failed"
  if (!condition) stop(message)
}

checkBinaryTrait <- function(v, naVal = NULL) {
  #if( !is.numeric(v) ) stop("Only numeric vectors are accepted.")
  # remove NA's
  v2 <- na.omit(v)
  # get unique values
  v_unique <- unique(v2)
  # remove 'naVal's
  v_unique2 <- v_unique[! v_unique %in% naVal]
  # count number of unique values and check whether all values are integers
  if ( length(unique(v_unique2)) == 2L) TRUE else FALSE
}

checkClassImbalance <- function(v, threshold = .40) {
  suppressMessages(require(MASS))
  t <- as.vector(table(v))
  #set Majority (M) and minority (m) class
  if (t[1] > t[2]) {
    M <- t[1]
    m <- t[2]
  } else {
    M <- t[2]
    m <- t[1]
  }
  if (M==0) stop("check to make sure your input classes don't equal 0")
  ratio <- m/M
  if (ratio > threshold) return(list(ratio = ratio, fraction = MASS::fractions(ratio), imbalanced = FALSE)) else return(list(ratio = ratio,  fraction = MASS::fractions(ratio), imbalanced = TRUE))
}

checkNewData <- function(object, newdata) {
  discrep <- setdiff(names(newdata), names(object$data$train))
  discrep
}

coerceToBinary <- function(v, fun = median, is_binary = checkBinaryTrait(v)) {
  suppressMessages(require(forcats))
  ## should only take a vector as input
  ## if value is already binary, just return it
  if (is_binary) {
    #make sure factor level orders are appropriately set
    v <- forcats::fct_inorder(factor(v))
    #make sure we have valid names
    levels(v) <- make.names(levels(v))
    return(v)
  }
  else if (is.numeric(v) && !is.factor(v)) {
    #determine threshold value in order to binarize
    #currently just takes rounded median but can be set with the FUN parameter
    threshold <- round(fun(unique(v)),0)
    factor(sapply(v, function(x)
      if (x < threshold) {
        paste("lt", as.character(threshold), sep="")
      } else
        paste("gte", as.character(threshold), sep="")
    ))
  }
  else {
    # if we can't binarize, return FALSE
    FALSE
  }
}


preprocessDummifyCenterScaleNzv <- function(df, y, pp_method_list = pp_method_list,  predict.autoCaret=FALSE){
  if (missing(y)) stop("Y must be specified")
  if (predict.autoCaret) {
    target<- df[[deparse(y)]]
    df[[deparse(y)]] <- NULL
  } else {
    target<- df[[deparse(substitute(y))]]
    df[[deparse(substitute(y))]] <- NULL
  }
  #check to ensure that columns have at least 2 unique values
  nonStatic <- sapply(df, function(col) length(unique(col))) > 1
  #save column names
  colNames <- colnames(df)
  #split data
  tempdf <- data.frame(df[, nonStatic])
  restdf <- data.frame(df[, !nonStatic])
  #replace lost column names
  colnames(tempdf) <- colNames[nonStatic]
  colnames(restdf) <- colNames[!nonStatic]

  # turn on fullRank by default to avoid the dummary variable trap
  dmy <- caret::dummyVars(" ~ .", data = tempdf, , fullRank=T)
  tempdf <- data.frame(predict(dmy, newdata = tempdf))
  df <- cbind(tempdf, restdf)
  preProc <- caret::preProcess(df, method = pp_method_list)
  if (predict.autoCaret) {
    return(data.frame(predict(preProc, df)))
  } else {
    return(data.frame(predict(preProc, df), y = target))
  }
}

progressWrapper <- function(message="", pb=PB, time=0, verbose = progressBar, sleeptime=2) {
  Sys.sleep(sleeptime)
  if (verbose==TRUE) {
    flush.console()
    cat('\r', utils::setTxtProgressBar(pb, time))
    cat('\r', message)
  }
}

ZeroAndOne <- function(field) {
  min <- sort(unique(field))[1]
  max <- sort(unique(field))[2]
  field[field == min] <- 0
  field[field == max] <- 1
  return(field)
}


getDummifiedVariables <- function(df, df_processed){
  df_non_factor_cols <- df[!sapply(df,is.factor)]
  df_non_factor_colnames <- names(df_non_factor_cols)
  df_non_factor_colnames <- c(df_non_factor_colnames,"y")
  df_processed_Cols_to_ReDummy <- df_processed[!colnames(df_processed) %in% df_non_factor_colnames]

  df_processed_ReDummied <- apply(df_processed_Cols_to_ReDummy,2,ZeroAndOne)
  y <- df$y
  cbind(df_non_factor_cols,df_processed_ReDummied,y)
}

