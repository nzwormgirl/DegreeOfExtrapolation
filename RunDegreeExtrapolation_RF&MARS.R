##############################################################
#   Model fitting and testing; interpolation-extrapolation   #
##############################################################

# IMPORTANT; Must be run using 32bit R. 
# install.packages("gradientForest", repos="http://R-Forge.R-project.org")


# Packages
library(randomForestSRC)                # for random forest fitting including many detailed outputs
library(gradientForest)                 # Must be using 32bit R.
library(earth)                          # for MARS models
library(dplyr)                          # for air quality dataset
library(lattice)                        # for trellis plots


source("DegreeofExtrapolation_functions.R")

##############################################################
#                       Input data                           #
##############################################################

# NZ datasets
load("FittedDataList.RData")

# a generic test dataset
# RFInterpExterp requires that all NA's are removed from its input dataframes.
airquality <- airquality %>% na.omit

##############################################################
# Run example, generic, random forest degree of extrapolation#
##############################################################

# fit to all data except month 5, predicted to month 5. 
DoEframeRF_airquality <- RFInterpExterp(myY = "Ozone", # response variable
                           myX = c("Solar.R", "Wind", "Temp"), # predictor variables
                           myFrame = airquality[airquality$Month != 5, ], # training data
                           myFrameOutside = airquality[airquality$Month == 5, ], # new data
                           ntree = 500, importance = "permute") # random forest settings

# histogram showing distribution of degree of extrapolation
histogram(~DegreeExtrap|Source, data = DoEframeRF_airquality$WeightFrame, col = "white", layout = c(1,2))


##############################################################
#   Run example, NZ, random forest degree of extrapolation   #
##############################################################

# set response variable of interest
myVar <- "FRE3"
head(FittedDataList[[myVar]])

#  calculate degree of extrapolation
DoEframeRF_FRE3 <- RFInterpExterp(myY = names(FittedDataList[[myVar]])[1], # response variable
                                  myX = names(FittedDataList[[myVar]])[3:10], # predictor variables
                                  myFrame = FittedDataList[[myVar]] [FittedDataList[[myVar]]$Island == "South", ], # training data
                                  myFrameOutside = FittedDataList[[myVar]] [FittedDataList[[myVar]]$Island == "North", ],   # new data
                                  ntree = 500, importance = "permute") # random forest settings

# add some variables for plotting
MapVars <- c("Island","segYmidpoint","segXmidpoint")

# add coordinate data for mapping
DoEframeRF_FRE3$WeightFrame[, MapVars] <- FittedDataList[[myVar]][ match(rownames(DoEframeRF_FRE3$WeightFrame) , rownames(FittedDataList[[myVar]])) , MapVars]

# histogram showing distribution of degree of extrapolation
histogram(~DegreeExtrap|Source, data = DoEframeRF_FRE3$WeightFrame, col = "white", layout = c(1,2))

# a map of degree of extrapolation.
trellis.par.set(name = "superpose.symbol", value = list(col = rainbow(12), pch = 16))
xyplot(segYmidpoint~ segXmidpoint|Source, data = DoEframeRF_FRE3$WeightFrame, groups = cut(DegreeExtrap, breaks=10), auto.key = list(space = "right"), scales = list(draw = F))



##############################################################
#     Run example, generic, MARS degree of extrapolation     #
##############################################################

# train a MARS model and calculate degree of extrapolaiton. 
# Train to all data except month 5, predicted to month 5. 

DoEframeMARS_airquality <- EarthInterpExterp(myY = "Ozone", # response variable
                              myX = c("Solar.R", "Wind", "Temp"), # predictor variables 
                              myFrame = airquality[airquality$Month != 5, ], # training data
                              myFrameOutside = airquality[airquality$Month == 5, ]) # new data

# predictions vs degree of extrapolation
xyplot(DegreeExtrap~PredBlind, groups = Source, data = DoEframeMARS_airquality$WeightFrame,auto.key = list(space = "right"))



##############################################################
#        Run example, NZ, MARS degree of extrapolation       #
##############################################################

myVar <- "FRE3"
DoEframeMARS_FRE3 <- EarthInterpExterp(myY = names(FittedDataList[[myVar]])[1], # response variable
                              myX = names(FittedDataList[[myVar]])[3:10], # predictor variables 
                              myFrame = FittedDataList[[myVar]][FittedDataList[[myVar]]$Island == "South", ], # training data
                              myFrameOutside = FittedDataList[[myVar]][FittedDataList[[myVar]]$Island == "North", ]) # new data

# predictions vs degree of extrapolation
xyplot(DegreeExtrap~PredBlind, groups = Source, data = DoEframeMARS_FRE3$WeightFrame,auto.key = list(space = "right"))


