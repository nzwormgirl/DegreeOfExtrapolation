##############################################################
#   Model fitting and testing; interpolation-extrapolation   #
##############################################################


##############################################################
#                       Functions                            #
##############################################################

#' This function allows you to fit a random forest regression model and assess degree of extrapolation when predicting to a new set of data 
#' @param myY Name of response variable (must be a column name of numeric columns of myFrame).
#' @param myX Vector of predictor variable names (must be column names of numeric columns of myFrame).
#' @param myFrame A dataframe for the random forest to fit to.
#' @param myFrameOutside A dataframe for the random forest to predict to.
#' @param ... Further arguments to be used in rfsrc function
#' @keywords random forest
#' @export
#' @examples
#' RFInterpExterp()
#' @return  a list containing the following two elements. The first element (called RFmodel) is the output object from rfsrc. 
#' The second element (called WeightFrame) is a dataframe containing three columns. PredBlind contains the predicted values.
#' Source identifies between OOB and new predictions.
#' DegreeExtrap contains the Degree of Extrapolation describing tendency towards extrapolation (positive values) versus interpolation (negative values). 
#' @details RFInterpExterp requires the response and all predictor to be continuous numeric variables, each containing no NA values.

#########
RFInterpExterp <- function(myY, myX, myFrame, myFrameOutside, ...) {   # 
  
  # Packages
  require(randomForestSRC)                # for random forest fitting including many detailed outputs
  require(gradientForest)                 # Must be using 32bit R. For gradient forests and extended forests to get x transformed to turnover in y space
  
  if(any(sapply(myFrame[, c(myY, myX)],    function(x) is.na(x)))) stop("NA's found in myFrame")
  if(any(sapply(myFrameOutside[, c( myX)], function(x) is.na(x)))) stop("NA's found in myFrameOutside")
  
  # rownames must represent numbers
  InsideRowNames <- rownames(myFrame)
  OutsideRowNames <- rownames(myFrameOutside)
  rownames(myFrame) <- as.character(1:nrow(myFrame))
  rownames(myFrameOutside) <- as.character(1:nrow(myFrameOutside))

  #########
  # function to rbind all the elements in a list of matrices
  # dealing with nonunique rownames
  # return dataframe
  Doug.rbind.matrix <-  function (dfs){
    myOut <- do.call("rbind", lapply(dfs, function(df) df))
    rownames(myOut) <- 1:nrow(myOut)
    myOut <- data.frame(myOut)
    return(myOut)
  }  
  #########

  print(paste("randomForest for", myY))
  
  RFmodel <- rfsrc(formula(paste(myY, paste(myX, collapse = "+"), sep = "~")), data = myFrame, membership = T, ...) #  ntree = 100, importance = "permute"
  
  rownames(RFmodel$membership) <- rownames(myFrame)# terminal node for each obs
  rownames(RFmodel$inbag) <-      rownames(myFrame)# number of times each obs is in each tree

  ######### 
  # ObsNumber = vector of rownames of observations
  # n = tree number
  GetNodeWeighting <- function(ObsNumber, nTree){
    myInbag <-     RFmodel$inbag[ , nTree]         # vector; how many times are obs inbag for that tree  
    Inbag <- myInbag[names(ObsNumber)]             # isolate obs of interest
    WeightInbag <- Inbag/sum(Inbag)                # weightings of observations used in this tree to get this prediction
    ObsValue <- myFrame[ObsNumber, myY]            # observed values of y
    myOut <- cbind(ObsNumber, Inbag, WeightInbag, nTree, ObsValue) 
    return(myOut)
  }
  #########
  
  #########
  # n = tree number
  GetNodeMembership <- function(n) {
    myVec <- RFmodel$membership[ , n]                              # Matrix recording terminal node membership where each column contains the node number that a case falls in for that tree. Irrespective of whether inbag.
    myVecUnique <- unique(myVec)                                   # node numbers of terminal nodes for that tree
    names(myVecUnique) <- myVecUnique
    myList <- lapply(myVecUnique, function(x) which(x == myVec) )  # which obs are contained within each node
    myOut <- lapply(myList, GetNodeWeighting, nTree = n)
    return(myOut)
  }
  #########
  
  print(paste("WeightList for", myY))
  # get weights attributed to  each observation for each terminal node for each tree. out-of-bag obs have no weighting
  WeightList <-  lapply(1:RFmodel$ntree, GetNodeMembership)  # WeightList[[tree]][[TerminalNode]]; matrix
  names(WeightList) <- 1:RFmodel$ntree
  
  # what weighting at which obs has been used to make each prediction
  #########
  GetW1 <- function(myObsName) {
    myOut <- Doug.rbind.matrix(lapply(1:ncol(RFmodel$membership), GetW2, myObsName = myObsName))
    myOut$ObsName <- rownames(myFrame)[myOut$ObsNumber]
    return(myOut)
  }
  #########
  
  #########
  GetW2 <- function(myTree, myObsName) {
    myNode <- as.character(RFmodel$membership[myObsName , myTree])
    myOut <- WeightList[[myTree]][[myNode]]
    myOut <- myOut[myOut[, "WeightInbag"] != 0, ]              # remove obs that were not in bag for that tree
    return(myOut)
  }
  #########
  
  # for each obs of fitted data (a row of myFrame), a dataframe containing the weigthings and observtions used to make predictions   
  print(paste("WeightList2 for", myY))
  WeightList2 <- lapply(rownames(RFmodel$membership), GetW1)   # WeightList2[[FittedObs]]; matrix
  names(WeightList2) <- rownames(RFmodel$membership)
  
  # for each observation, what are the weightings on each of the observations used to make predictions, with adjustment for number of terminal nodes in each tree
  #########
  # similar, but weightings from out-of-bag trees only
  GetOOBweights <- function(myObsName) {
    myInbag <- RFmodel$inbag[myObsName, ]      # how many times is that obs in-bag for each tree
    myOOB <- which(myInbag == 0)
    x <- WeightList2[[myObsName]]              # for this obs, the terminal nodes that are at the bottom of each tree
    x$OOB <- sapply(x$nTree, function(x) any(x == myOOB))
    myAll <- tapply(x$WeightInbag,  factor(x$ObsName ,        levels = rownames(myFrame)), sum, na.rm = T) # get weightings for all trees
    myAll <- myAll / RFmodel$ntree                    # standardise by number of trees
    myOut <- tapply(x$WeightInbag[x$OOB],  factor(x$ObsName[x$OOB],        levels = rownames(myFrame)), sum, na.rm = T) # get weightings for OOB trees
    myOut <- myOut / length(unique(x$nTree [x$OOB]))  # standardise by number of trees
    return(cbind(myOut, myAll))
  }
  #########
  
  # for each observation, what are the weightings on each of the observations used to make predictions, with adjustment for number of terminal nodes in each tree
  print(paste("WeightingMatInsideOOB for", myY))
  Doug <- lapply(names(WeightList2), GetOOBweights)
  WeightingMatInsideOOB   <- sapply(Doug, function(x) x[,"myOut"])    # this is weightings from out-of-bag only
  rm(Doug, WeightList2); gc()
  
  # add predictions
  myFrame$PredFitted <- as.vector(RFmodel$predicted)       # fitted predictions
  myFrame$PredBlind <-  as.vector(RFmodel$predicted.oob)    # blind predictions from OOB cases
  
  #######################################################
  
  # get similar to WeightingMatInside, but for new predictions (for myFrameOutside)
  myPredOutside <- predict(RFmodel, newdata = myFrameOutside, membership = T)
  rownames(myPredOutside$membership) <- rownames(myPredOutside$xvar)
  
  #########
  GetW3 <- function(myNode, myTree) {
    myNode <- myPredOutside$membership[myNode,myTree]                # terminal node for this prediction, for this tree
    myOut <- WeightList[[myTree]][[as.character(myNode)]]            # the set of information used to get predicted value for this pred for this tree
    PredNameTo <- as.numeric(rep(names(myNode), nrow(myOut)))        # rowname being predicted to
    myOut <- cbind(myOut, PredNameTo) #  PredName,  ObsName, 
    myOut <- myOut[myOut[, "WeightInbag"] != 0, ]                    # dont want obs that where not in bag for that tree
    return(myOut)
  }
  #########
  
  #########
  GetW4 <- function(myTree) {
    myOut <- lapply(1:nrow(myPredOutside$membership), GetW3, myTree = myTree) # Doug.rbind.matrix()
    return(myOut)
  }
  #########
  
  print(paste("WeightListOutside for", myY))
  WeightListOutside <- lapply(1:RFmodel$ntree, GetW4 ) # Doug.rbind.list()         
  print(paste("rbinding WeightListOutside for", myY))
  Doug <- lapply(WeightListOutside, function(dfs) do.call("rbind", lapply(dfs, function(df) df)))
  Doug <- do.call("rbind", lapply(Doug, function(df) df))
  ObsName <- rownames(myFrame)[Doug[, "ObsNumber"]]
  # weightings on the fitted observations (rows) that are to be used for the new predictions (columns), standardised by number of trees
  print(paste("tapplying WeightListOutside for", myY))
  WeightingMatOutside <- tapply(Doug[,"WeightInbag"], list( factor(ObsName, levels = rownames(myFrame)), Doug[,"PredNameTo"]), sum, na.rm = T)/ RFmodel$ntree

  WeightingMatOutside <- WeightingMatOutside[, rownames(myFrameOutside)]
  rm(Doug, ObsName, WeightList, WeightListOutside); gc()
  
  # add predictions (they are blind to fitted data)
  myFrameOutside$PredBlind <- as.vector(myPredOutside$predicted)
  
  # run a gradient forest to get prediction (X w.r.t. y) (could also do other transformations of X)
  print(paste("gradientForest for", myY))
  GFmodel <- gradientForest(myFrame, predictor.vars = myX, response.vars = myY, mtry = RFmodel$mtry, ntree = RFmodel$ntree )  
  GFpredInside <-  predict(GFmodel, extrap = T) #  It is a dataframe in which each predictor has been transformed to the biological scale by the cumulative importance function, as defined by cumimp.

  # distances in X with respect to y for fitted data. Could use various methods. using default method = "euclidean"
  myFrame$WeightedDistBlind    <-      apply(t(WeightingMatInsideOOB)   * as.matrix(dist(GFpredInside)), 2, sum, na.rm = T) # distances weighted by oob trees only
  
  # for each ob, what is the WeightedDistBlind of the observations that make up its predicted value
  myFrame$WeightedDist2step <- apply(WeightingMatInsideOOB, 2, function(x) sum(x * myFrame$WeightedDistBlind, na.rm = T) )
  
  # for each new prediction, distances (in X with respect to y adjusted by weighting of RF) between each each new ob and all fitted obs. 
  GFpredOutside <- predict(GFmodel, newdata = myFrameOutside[, myX], extrap = T)
  # dont need distances between all sets, only distances between each GFpredOutside and all GFpredInside (no distances within GFpredOutside required)
  #########
  GetWeightDist <- function(n) {
    ThisDist <- dist(rbind(GFpredOutside[n, ], GFpredInside))
    ThisDistWeighted <-  WeightingMatOutside[, n] * as.matrix(ThisDist)[1,-1]
    myOut <- sum(ThisDistWeighted, na.rm=T)
    return(myOut)
  }
  #########
  myFrameOutside$WeightedDistBlind <- sapply(1:nrow(myFrameOutside), GetWeightDist)
  
  # summed weighted distances of the ones used to get prediction for each new observation. distance for oob trees vs distance for all trees 
  myFrameOutside$WeightedDist2step <- sapply(1:nrow(myFrameOutside) , function(x) sum( myFrame$WeightedDistBlind * WeightingMatOutside[, x] , na.rm = T) )
  
  myFrame$Source        <- "Out of bag" 
  myFrameOutside$Source <- "New data"
  
  myOutVars <- c("PredBlind","Source", "WeightedDistBlind", "WeightedDist2step") 

  # re-install rownames
  rownames(myFrame) <- InsideRowNames 
  rownames(myFrameOutside) <-OutsideRowNames

  myOutFrame <- rbind(myFrame[, myOutVars], myFrameOutside[, myOutVars])
  
  # degree of extrapolation
  myOutFrame$DegreeExtrap <- (myOutFrame$WeightedDistBlind / myOutFrame$WeightedDist2step) -1
  
  # organise factor levels 
  myOutFrame$Source <- factor(myOutFrame$Source, levels = c("Out of bag","New data"))
  
  return(list(RFmodel = RFmodel, WeightFrame = myOutFrame[, c("PredBlind","Source","DegreeExtrap")]))  
}
#########


##############################################################
#                       Input data                           #
##############################################################

load("FittedDataList.RData")


##############################################################
#                    Run example, NZ                         #
##############################################################

head(FittedDataList[[1]])

myVar <- "FRE3"
DoEframe <- RFInterpExterp(myY = names(FittedDataList[[myVar]])[1], 
                           myX = names(FittedDataList[[myVar]])[3:10], 
                           myFrame        = FittedDataList[[myVar]] [FittedDataList[[myVar]]$Island == "South", ], 
                           myFrameOutside = FittedDataList[[myVar]] [FittedDataList[[myVar]]$Island == "North", ], 
                           ntree = 500, importance = "permute")

# add some variables for plotting
MapVars <- c("Island","segYmidpoint","segXmidpoint")

DoEframe$WeightFrame[, MapVars] <- FittedDataList[[myVar]][ match(rownames(DoEframe$WeightFrame) , rownames(FittedDataList[[myVar]])) , MapVars]

# histogram showing distribution of degree of extrapolation
histogram(~DegreeExtrap|Source, data = DoEframe$WeightFrame, col = "white", layout = c(1,2))

# a map of degree of extrapolation.
library(lattice)
trellis.par.set(name = "superpose.symbol", value = list(col = rainbow(12), pch = 16))
xyplot(segYmidpoint~ segXmidpoint|Source, data = DoEframe$WeightFrame, groups = cut(DegreeExtrap, breaks=10), auto.key = list(space = "right"), scales = list(draw = F))


##############################################################
#                  Run example, generic                      #
##############################################################

library(dplyr)

# RFInterpExterp requires that all NA's are removed from its input dataframes.
airquality <- airquality %>% na.omit

# fit to all data except month 5, predicted to month 5. 
DoEframe <- RFInterpExterp(myY = "Ozone", 
                           myX = c("Solar.R", "Wind", "Temp"), 
                           myFrame        = airquality[airquality$Month != 5, ], 
                           myFrameOutside = airquality[airquality$Month == 5, ], 
                           ntree = 500, importance = "permute")

# histogram showing distribution of degree of extrapolation
histogram(~DegreeExtrap|Source, data = DoEframe$WeightFrame, col = "white", layout = c(1,2))

