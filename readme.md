<!-- README.md is generated from README.Rmd. Please edit that file -->

autoCaret: make machine learning easier!
---------------
**Authors:** [Greg Ceccarelli](https://www.linkedin.com/in/gregceccarelli), [Michael Marks](https://www.linkedin.com/in/michael-marks-58948636), [Rock Baek](https://www.linkedin.com/in/rock-baek-61588623)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)
[![Travis-CI Build Status](https://travis-ci.org/gregce/autoCaret.svg?branch=master)](https://travis-ci.org/gregce/autoCaret)

This vignette is designed to introduce you to the `autoCaret` R package. This package is built on top of both the [caret](https://cran.r-project.org/web/packages/caret/) and [caretEnsemble](https://cran.r-project.org/web/packages/caretEnsemble/) R packages for machine learning and can take as input an R dataframe suitable for binary classification. Currently, [binary classification](https://en.wikipedia.org/wiki/Binary_classification), is the primary purpose of `autoCaret`.

The main function, `automodel`, will do the following:

  1. Validate the input data frame - binarizing the target variable if possible. 
  2. Split the dataframe into both a training and test set
  3. Preprocess the input data - center, scale and remove variables with zero variance
  3. Check for class imbalance and attempt to downsample, as default, to help combat poor predictive accuracy
  4. Build a suite of models determined by the `methodlist` parameter. If NULL, defaults to "glm", "rpart", rf" &"xgbLinear""
  5. Blend the models into an ensemble model using the `caretEnsemble` package
  6. Use the ensemble to make predictions on the held out test set
  7. Return, to the end user, an `autoCaret` model object with a variety of useful pieces of metadata about the modeling process
  
The `autoCaret` model object is fully accessible and can be summarized using the the `summary()` generic function. 

Additionally, predictions can be made using the `predict()` generic function passing in as parameters an `autoCaret` object and new data.

---

## Installation

At the time of this writing, this package is not hosted on CRAN, but can be obtained from GitHub. To do so, first make sure you have [devtools](https://cran.r-project.org/web/packages/devtools/index.html) installed.


```r
install.packages("devtools")
```

Now we can install from GitHub using the following line:


```r
devtools::install_github("gregce/autoCaret")
```

Once the `autoCaret` package is installed, you may access its functionality as you would any other package by calling:


```r
library("autoCaret")
```

If all went well, check out the vignette("autoCaret") which will pull up this vignette!

## Basic Usage

We begin by loading the `mlbench` pakacage and some example data, Sonar, which is commonly used to demostrate machine learning functionality. In this example, we will attempt to distinguish Mines (M) from Rocks (R) using binary classification with an initial dataset of where N=208 and P=60.  

As a general rule, when using `autoCaret::autoModel` defaults, datasets less than 100mb should yield optimal performance. and in order to avoid extremely long run times and/or high memory requirements. 


```r
library(mlbench)
library(autoCaret)

# Load the data into Memory from the mlbench package
data("Sonar")

# Take a brief peak at the Sonar dataframe
dplyr::glimpse(Sonar)
```

```
## Observations: 208
## Variables: 61
## $ V1    <dbl> 0.0200, 0.0453, 0.0262, 0.0100, 0.0762, 0.0286, 0.0317, ...
## $ V2    <dbl> 0.0371, 0.0523, 0.0582, 0.0171, 0.0666, 0.0453, 0.0956, ...
## $ V3    <dbl> 0.0428, 0.0843, 0.1099, 0.0623, 0.0481, 0.0277, 0.1321, ...
## $ V4    <dbl> 0.0207, 0.0689, 0.1083, 0.0205, 0.0394, 0.0174, 0.1408, ...
## $ V5    <dbl> 0.0954, 0.1183, 0.0974, 0.0205, 0.0590, 0.0384, 0.1674, ...
## $ V6    <dbl> 0.0986, 0.2583, 0.2280, 0.0368, 0.0649, 0.0990, 0.1710, ...
## $ V7    <dbl> 0.1539, 0.2156, 0.2431, 0.1098, 0.1209, 0.1201, 0.0731, ...
## $ V8    <dbl> 0.1601, 0.3481, 0.3771, 0.1276, 0.2467, 0.1833, 0.1401, ...
## $ V9    <dbl> 0.3109, 0.3337, 0.5598, 0.0598, 0.3564, 0.2105, 0.2083, ...
## $ V10   <dbl> 0.2111, 0.2872, 0.6194, 0.1264, 0.4459, 0.3039, 0.3513, ...
## $ V11   <dbl> 0.1609, 0.4918, 0.6333, 0.0881, 0.4152, 0.2988, 0.1786, ...
## $ V12   <dbl> 0.1582, 0.6552, 0.7060, 0.1992, 0.3952, 0.4250, 0.0658, ...
## $ V13   <dbl> 0.2238, 0.6919, 0.5544, 0.0184, 0.4256, 0.6343, 0.0513, ...
## $ V14   <dbl> 0.0645, 0.7797, 0.5320, 0.2261, 0.4135, 0.8198, 0.3752, ...
## $ V15   <dbl> 0.0660, 0.7464, 0.6479, 0.1729, 0.4528, 1.0000, 0.5419, ...
## $ V16   <dbl> 0.2273, 0.9444, 0.6931, 0.2131, 0.5326, 0.9988, 0.5440, ...
## $ V17   <dbl> 0.3100, 1.0000, 0.6759, 0.0693, 0.7306, 0.9508, 0.5150, ...
## $ V18   <dbl> 0.2999, 0.8874, 0.7551, 0.2281, 0.6193, 0.9025, 0.4262, ...
## $ V19   <dbl> 0.5078, 0.8024, 0.8929, 0.4060, 0.2032, 0.7234, 0.2024, ...
## $ V20   <dbl> 0.4797, 0.7818, 0.8619, 0.3973, 0.4636, 0.5122, 0.4233, ...
## $ V21   <dbl> 0.5783, 0.5212, 0.7974, 0.2741, 0.4148, 0.2074, 0.7723, ...
## $ V22   <dbl> 0.5071, 0.4052, 0.6737, 0.3690, 0.4292, 0.3985, 0.9735, ...
## $ V23   <dbl> 0.4328, 0.3957, 0.4293, 0.5556, 0.5730, 0.5890, 0.9390, ...
## $ V24   <dbl> 0.5550, 0.3914, 0.3648, 0.4846, 0.5399, 0.2872, 0.5559, ...
## $ V25   <dbl> 0.6711, 0.3250, 0.5331, 0.3140, 0.3161, 0.2043, 0.5268, ...
## $ V26   <dbl> 0.6415, 0.3200, 0.2413, 0.5334, 0.2285, 0.5782, 0.6826, ...
## $ V27   <dbl> 0.7104, 0.3271, 0.5070, 0.5256, 0.6995, 0.5389, 0.5713, ...
## $ V28   <dbl> 0.8080, 0.2767, 0.8533, 0.2520, 1.0000, 0.3750, 0.5429, ...
## $ V29   <dbl> 0.6791, 0.4423, 0.6036, 0.2090, 0.7262, 0.3411, 0.2177, ...
## $ V30   <dbl> 0.3857, 0.2028, 0.8514, 0.3559, 0.4724, 0.5067, 0.2149, ...
## $ V31   <dbl> 0.1307, 0.3788, 0.8512, 0.6260, 0.5103, 0.5580, 0.5811, ...
## $ V32   <dbl> 0.2604, 0.2947, 0.5045, 0.7340, 0.5459, 0.4778, 0.6323, ...
## $ V33   <dbl> 0.5121, 0.1984, 0.1862, 0.6120, 0.2881, 0.3299, 0.2965, ...
## $ V34   <dbl> 0.7547, 0.2341, 0.2709, 0.3497, 0.0981, 0.2198, 0.1873, ...
## $ V35   <dbl> 0.8537, 0.1306, 0.4232, 0.3953, 0.1951, 0.1407, 0.2969, ...
## $ V36   <dbl> 0.8507, 0.4182, 0.3043, 0.3012, 0.4181, 0.2856, 0.5163, ...
## $ V37   <dbl> 0.6692, 0.3835, 0.6116, 0.5408, 0.4604, 0.3807, 0.6153, ...
## $ V38   <dbl> 0.6097, 0.1057, 0.6756, 0.8814, 0.3217, 0.4158, 0.4283, ...
## $ V39   <dbl> 0.4943, 0.1840, 0.5375, 0.9857, 0.2828, 0.4054, 0.5479, ...
## $ V40   <dbl> 0.2744, 0.1970, 0.4719, 0.9167, 0.2430, 0.3296, 0.6133, ...
## $ V41   <dbl> 0.0510, 0.1674, 0.4647, 0.6121, 0.1979, 0.2707, 0.5017, ...
## $ V42   <dbl> 0.2834, 0.0583, 0.2587, 0.5006, 0.2444, 0.2650, 0.2377, ...
## $ V43   <dbl> 0.2825, 0.1401, 0.2129, 0.3210, 0.1847, 0.0723, 0.1957, ...
## $ V44   <dbl> 0.4256, 0.1628, 0.2222, 0.3202, 0.0841, 0.1238, 0.1749, ...
## $ V45   <dbl> 0.2641, 0.0621, 0.2111, 0.4295, 0.0692, 0.1192, 0.1304, ...
## $ V46   <dbl> 0.1386, 0.0203, 0.0176, 0.3654, 0.0528, 0.1089, 0.0597, ...
## $ V47   <dbl> 0.1051, 0.0530, 0.1348, 0.2655, 0.0357, 0.0623, 0.1124, ...
## $ V48   <dbl> 0.1343, 0.0742, 0.0744, 0.1576, 0.0085, 0.0494, 0.1047, ...
## $ V49   <dbl> 0.0383, 0.0409, 0.0130, 0.0681, 0.0230, 0.0264, 0.0507, ...
## $ V50   <dbl> 0.0324, 0.0061, 0.0106, 0.0294, 0.0046, 0.0081, 0.0159, ...
## $ V51   <dbl> 0.0232, 0.0125, 0.0033, 0.0241, 0.0156, 0.0104, 0.0195, ...
## $ V52   <dbl> 0.0027, 0.0084, 0.0232, 0.0121, 0.0031, 0.0045, 0.0201, ...
## $ V53   <dbl> 0.0065, 0.0089, 0.0166, 0.0036, 0.0054, 0.0014, 0.0248, ...
## $ V54   <dbl> 0.0159, 0.0048, 0.0095, 0.0150, 0.0105, 0.0038, 0.0131, ...
## $ V55   <dbl> 0.0072, 0.0094, 0.0180, 0.0085, 0.0110, 0.0013, 0.0070, ...
## $ V56   <dbl> 0.0167, 0.0191, 0.0244, 0.0073, 0.0015, 0.0089, 0.0138, ...
## $ V57   <dbl> 0.0180, 0.0140, 0.0316, 0.0050, 0.0072, 0.0057, 0.0092, ...
## $ V58   <dbl> 0.0084, 0.0049, 0.0164, 0.0044, 0.0048, 0.0027, 0.0143, ...
## $ V59   <dbl> 0.0090, 0.0052, 0.0095, 0.0040, 0.0107, 0.0051, 0.0036, ...
## $ V60   <dbl> 0.0032, 0.0044, 0.0078, 0.0117, 0.0094, 0.0062, 0.0103, ...
## $ Class <fctr> R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R...
```

Having both the data loaded and having inspected it, we can now make use of the `autoCaret::autoModel()` function As stated above, we intend to try and distinguish Rocks (R) from Mines (R), so we will attempt to predict the `Class` variable in the Sonar dataframe.

Using it's defaults, `autoModel` has 2 arguments we need to specify: `df` and `y`.

`df` is the Dataframe that we'd like to use build a binary classification model, while `y` is our classification target or response variable. We can use a non-exported package function, `autoCaret:::checkBinaryTrait` to determine if our `y` variable is indeed binary. The `autoModel` functionality will perform this for us as well.



```r
# Manually check that our intended y paramter is indeed binary
autoCaret:::checkBinaryTrait(Sonar$Class)
```

```
## [1] TRUE
```

```r
# Generate an autoCaret object using the autoModel function
mod <- autoCaret::autoModel(df = Sonar, y = Class, progressBar = FALSE)
```

In the example above, the returned object, `mod`, is an `autoCaret` object containing 16 objects. To confirm, we can run the below two commmands:


```r
# Check class of autoCaret object
class(mod)
```

```
## [1] "autoCaret"
```

```r
# High level 
nrow(summary.default(mod))
```

```
## [1] 19
```

Running the summary function on our model output displays a wealth of information about the contents of the object as well as the procedural steps taken during modeling. In our example, we observe:

 - that our initial dataset of 208 observation was split into a training and test set containing 167 and 41 observations 
 - Modeling took .64 minutes and entailed resampling our dataset 10 times 
 - We used the four default models to create an ensemble.
 - Using the ensemble model that was generated to predict on the test set yield predictions with 92% accuracy. 


```r
# Use the summary generic to store a summary of autoCaret object
overview <- summary(mod)
```

```
## The input dataset had: 208 observations and 60 predictors 
## ---------------------
## Prior to model training, the input dataset was split into a training & test set 
## The training set has:  167  observations 
## The test set has:      41  observations 
## ---------------------
## Overall modeling took: 0.64 minutes 
## During that time the training data was boostrap resampled 10 times 
## 
## The following classification models were used to create an ensemble: 
## - glm
## - rpart
## - rf
## - xgbLinear
## To learn more about ensemble learing in the context of machine learning, please visit: https://en.wikipedia.org/wiki/Ensemble_learning 
## 
## In the ensemble, the top 5 variables in order from highest to lowest level of relative importance, were: 
## -  V11
## -  V12
## -  V9
## -  V10
## -  V36
## ---------------------
## When the ensemble model was used to predict on the held out test set of 41 observations it performed as follows: 
## 
## Overall Accuracy: 92.68
## 
## A confusion matrix demonstrating accuracy is as follows: 
##           Reference
## Prediction  R  M
##          R 18  2
##          M  1 20
## 
## Precision: 90
## Recall: 94.74
## ---------------------
## To learn more about Precision & Recall in the context of information retriveal, please visit: https://en.wikipedia.org/wiki/Precision_and_recall
```

We can also access each of object variables included in the above displayed summary output via the object itself.


```r
# Print the overview to the console
overview
```

```
## $input_row_count
## [1] 208
## 
## $input_col_count
## [1] 61
## 
## $train_row_count
## [1] 167
## 
## $test_row_count
## [1] 41
## 
## $modeling_time
## [1] 0.64
## 
## $number_resamples
## [1] 10
## 
## $method_list
## [1] "glm"       "rpart"     "rf"        "xgbLinear"
## 
## $confusionMatrix
##           Reference
## Prediction  R  M
##          R 18  2
##          M  1 20
## 
## $accuracy
## [1] 92.68
## 
## $sensitivity
## [1] 94.74
## 
## $specificity
## [1] 90.91
## 
## $precision
## [1] 90
## 
## $recall
## [1] 94.74
## 
## $best_model_results
##   model_name       ROC      Sens      Spec      ROCSD     SensSD
## 1   ensemble 0.9337900 0.8095031 0.8953181 0.01498452 0.01089121
## 2         rf 0.9052501 0.6901687 0.8829980 0.03551813 0.10603065
## 3  xgbLinear 0.8702916 0.7168320 0.8185311 0.04529174 0.10874655
## 4      rpart 0.7019527 0.6562942 0.7122567 0.08602055 0.15284623
## 5        glm 0.6451431 0.5910052 0.6543618 0.08914517 0.08671003
##       SpecSD
## 1 0.04371527
## 2 0.06784535
## 3 0.08614794
## 4 0.07226794
## 5 0.10772096
## 
## attr(,"class")
## [1] "summary.autoCaret"
```

## Predicting new data

So now that we have a sense of how successful our auto modeling approach was, we'd likely want to use the model, `mod`, we built previously to make predictions on new data we receive. 

Because this is an illustrative example, we'lll take a shortcut by just resampling the same data that we used to train on. The main point here is that you can simply pass your `autoCaret` model object, `mod`, into the `predict()` function along with new observations to generate predictions. 


```r
new <- Sonar[sample(1:nrow(Sonar), 50, replace=TRUE),]

#Make predicitons 
preds <- predict(mod, new)

#Print Predictions 
preds
```

```
##  [1] M R M R R R R M R M R M M R M R R M M R M M M M R M M M R R M R R M M
## [36] R M M M R R M M M R M M M M R
## Levels: R M
```

How well did we do? Well a confusion matrix from the `caret` package can tell us!

- We only mispredicted one example, for overall accuracy of .98
- **Note:** we wouldn't expect this level of accuracy using real data given we resampled from our original training set. 


```r
## How well did we do?
caret::confusionMatrix(data = preds, reference = new$Class)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  M  R
##          M 29  0
##          R  1 20
##                                           
##                Accuracy : 0.98            
##                  95% CI : (0.8935, 0.9995)
##     No Information Rate : 0.6             
##     P-Value [Acc > NIR] : 2.775e-10       
##                                           
##                   Kappa : 0.9587          
##  Mcnemar's Test P-Value : 1               
##                                           
##             Sensitivity : 0.9667          
##             Specificity : 1.0000          
##          Pos Pred Value : 1.0000          
##          Neg Pred Value : 0.9524          
##              Prevalence : 0.6000          
##          Detection Rate : 0.5800          
##    Detection Prevalence : 0.5800          
##       Balanced Accuracy : 0.9833          
##                                           
##        'Positive' Class : M               
## 
```

