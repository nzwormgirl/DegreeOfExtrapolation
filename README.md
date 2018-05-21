# Degree Of Extrapolation

Regression techniques are often used to predict responses across landscapes or under scenarios describing changes in climate, management or landcover. The ability of random forests (RF) and multivariate adaptive regression splines (MARS) to predict flow variability, low flow, Escherichia coli, and a macroinvertebrate community index across a national river network was compared. Cross-validation was applied to test predictive performance across an induced spectrum of interpolation to extrapolation by splitting each dataset into two geographical, environmental and random groups. RF and MARS are both capable of representing non-linear and interacting patterns, but showed contrasting ability to interpolate and extrapolate. Random-split cross-validation indicated RF always performed better than MARS when interpolating within environmental space. RF also performed better than MARS when extrapolating in geographical space. RF models for all four responses were transferable in geographic space, but not to environmental conditions outside the training data. Neither technique was successful when extrapolating across environmental gradients, although RF out-performed MARS, despite RF predictions being constrained by the training data. New methods to calculate degree of extrapolation to quantity interpolation versus extrapolation for each RF and MARS prediction are demonstrated. The methods use transformation of both the training data and new predictors in response turnover space. A decline in cross-validation performance was related to an increase in degree of extrapolation regardless of whether extrapolating in geographical or environmental space. High degree of extrapolation under climate or landcover change indicates increased risk of producing misleading predictions from both RF and MARS.

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
 
