# Degree Of Extrapolation

This repository hosts code and data to calculate the extent to which forest and multivariate adaptive regression spline (MARS) models extrapolate beyond the bounds of the training data (known as the degree of extrapolation). 

Note that the code for random forests must be run with 32bit R for the gradientForest package to run correctly. 

The repository contains 2 .R files and one .RData file containing the data used in the manuscript.
* *RunDegreeExtrapolation_RF&MARS.R* contains code to calculate the degree of extrapolation using random forest and MARS models for two datasets.
* *DegreeOfExtrapolation_functions.R* contains the functions required to calculate the degree of extrapolation using random forest and MARS models.
* *FittedDataList.RData* contains a New Zealand dataset that can be used to calculate the degree of extrapolation for four biophysical variables on the New Zealand river network.
