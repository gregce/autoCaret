% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoCaret.R
\name{autoModel}
\alias{autoModel}
\title{Wrapper function to allow for "1 Click Modeling"}
\usage{
autoModel(df, y, method_list = NULL, progressBar = TRUE,
  subsample = downSample, pp_method_list = c("center", "scale"))
}
\arguments{
\item{df}{The dataframe to use for binary predictions}

\item{y}{The response variable in the dataset}

\item{method_list}{Allows the user to pass in a list of models to use to make their ensemble, if NULL, the default is:}

\item{progressBar}{Allows the user to toggle verbose output about function execution (show a progress bar with other detail)}

\item{subsample}{Allows the user to pass in a \code{caret} subsampling function. Default = downSample.}
}
\value{
an \code{\link{autoCaret}} object
}
\description{
Wrapper function to allow for "1 Click Modeling"
}
\details{
The goal of this function is to allow for simple one click modeling of an example dataset.
The dataset should be of moderate size (nrow(df) < 1e6) and have a binary response variable (e.g. length(levels(df$y))==2). If a continuous variable is
passed in as the response parameter, the function will attempt to binarize it based on a distribution summarization function (median, mean, etc.)
Additionally, this function will automatically attempt to detect class imbalance and correct the input training dataset for the user.
Currently, this function does not support multiclass classification problems.
}
\examples{
\dontrun{
oneClick <- autoModel(Sonar, Class)
}
}

