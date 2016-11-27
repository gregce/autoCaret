## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("gregce/autoCaret")

## ---- eval=FALSE---------------------------------------------------------
#  library("autoCaret")

## ---- eval=TRUE----------------------------------------------------------
library(mlbench)
library(autoCaret)

# Load the data into Memory from the mlbench package
data("Sonar")

# Take a brief peak at the Sonar dataframe
dplyr::glimpse(Sonar)

## ----eval=TRUE, warning=FALSE, message=FALSE-----------------------------
# Check class of autoCaret object
class(mod)

# High level 
nrow(summary.default(mod))

## ----eval=TRUE, warning=FALSE, message=FALSE-----------------------------
# Use the summary generic to store a summary of autoCaret object
overview <- summary(mod)

## ----eval=TRUE, warning=FALSE, message=FALSE-----------------------------
# Print the overview to the console
overview

