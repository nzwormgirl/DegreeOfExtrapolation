# Degree Of Extrapolation

Regression techniques are often used to predict responses across landscapes or under scenarios describing changes in climate, management or landcover. Random forests (RF) and multivariate adaptive regression splines (MARS) are both capable of representing non-linear and interacting patterns, but show contrasting ability to interpolate and extrapolate. We provide new methods to calculate the degree of extrapolation to quantify interpolation versus extrapolation for each RF and MARS prediction. The methods use transformation of both the training data and new predictors in response turnover space. 

This repository hosts code and data to calculate the extent to which forest and multivariate adaptive regression spline (MARS) models extrapolate beyond the bounds of the training data (known as the degree of extrapolation). 

**Note that the code for random forests must be run with 32bit R for the gradientForest package to run.**

The repository contains 2 .R files and one .RData file containing the data used in the manuscript.
* *RunDegreeExtrapolation_RF&MARS.R* contains code to calculate the degree of extrapolation using random forest and MARS models for two datasets.
* *DegreeOfExtrapolation_functions.R* contains the functions required to calculate the degree of extrapolation using random forest and MARS models.
* *FittedDataList.RData* contains a New Zealand dataset that can be used to calculate the degree of extrapolation for four biophysical variables on the New Zealand river network.

The two main functions (*RFInterpExterp* for random forests and *EarthInterpExterp* for MARS models) take a dataframe containing a response variable and a set of predictive variables, with the data split into a training set and new data for which predictions are to be generated. 
* Both functions require the response and all predictor variables to be continuous numeric variables, each containing no NA values. 
* The *EarthInterpExterp* currently only works for earth models with no interactions (degree = 1).

These functions both return a list containing two elements:  
* The first element (*RFmodel* or *MARSmodel*) is the output object from the random forest or MARS model. 
* The second element (*WeightFrame*) is a dataframe containing three columns. 
  * *PredBlind* contains the predicted values.
  * *Source* identifies between OOB (out of bag - random forest) or LOO (leave one out - MARS) predictions and predictions to new data.
  * *DegreeExtrap* contains the Degree of Extrapolation describing tendency towards extrapolation (positive values) versus interpolation (negative values). 
 
