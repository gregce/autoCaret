## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("gregce/autoCaret")

## ---- eval=FALSE---------------------------------------------------------
#  library("autoCaret")

## ---- eval=TRUE, message=FALSE-------------------------------------------
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

## ----eval=TRUE, warning=FALSE, message=FALSE-----------------------------
new <- Sonar[sample(1:nrow(Sonar), 50, replace=TRUE),]

#Make predicitons 
preds <- predict(mod, new)

#Print Predictions 
preds

## ----eval=TRUE, warning=FALSE, message=FALSE-----------------------------
## How well did we do?
caret::confusionMatrix(data = preds, reference = new$Class)

