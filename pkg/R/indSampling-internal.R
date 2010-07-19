## Ian Kopacka
## 2010-07-16
##
## Function: indSampling.internal
## Package: FFD
## 
## For a fixed Population the number of herds to be tested, the
## expected number of animals to be tested and the costs are 
## computed for a sequence of herd sensitivities using "individual
## sampling".
##
## This internal function is called by 'indSamplingSummary.R'
##
## Input parameters:
##     survey.Data...Object of the class 'SurveyData', created by using
##                   the function 'surveyData.R'
##     herdSensVec...Numeric vector with values in (0,1). Herd sensitivities for
##                   which the number of herds to be tested, the expected total 
##                   number of animals to be tested and the expected costs are
##                   computed.
##
## Return value: Data frame with columns 'herdSensVec', 'nHerdsVec', 
##               'nAnimalsMeanVec' and 'expectedCostVec'.

## Internal auxilliary function:
indSampling.internal <- function(survey.Data, herdSensVec){
    
    ## Number of herds to be tested according to the
    ## herd sensitivities:
    nHerdsVec <- sapply(herdSensVec, 
        function(x) computeOptimalSampleSize(nPopulation = length(survey.Data@nAnimalVec), 
        prevalence = survey.Data@designPrevalence, alpha = survey.Data@alpha,
        sensitivity = x, specificity = 1, lookupTable = FALSE))
        
    ## Number of Animals to be tested:
#    meanSampleSize <- sapply(herdSensVec, 
#        function(x) computeAverageSampleSize(nAnimalVector = survey.Data@nAnimalVec,
#        intraHerdPrevalence = survey.Data@intraHerdPrevalence, herdSens = x, 
#        sensitivityDiagTest = survey.Data@diagSensitivity))
    meanSampleSize <- sapply(herdSensVec, 
        function(x){
            out <- computeSampleSizeInd(survey.Data = survey.Data, 
                herdSensitivity = x)
            return(out$nAnimalsMeanPerHerd)
        })        
    nAnimalsMeanVec <- meanSampleSize*nHerdsVec    
    
    ## Total Cost:
    if ((length(survey.Data@costHerd) > 0) & (length(survey.Data@costAnimal) > 0)){
        expectedCostVec <- nAnimalsMeanVec*survey.Data@costAnimal + 
            nHerdsVec*survey.Data@costHerd
    } else {
        expectedCostVec <- numeric(length(herdSensVec))*NA
    }
        
    ## Return value:
    out <- data.frame(herdSensVec = herdSensVec, 
        nHerdsVec = nHerdsVec,
        nAnimalsMeanVec = nAnimalsMeanVec, 
        expectedCostVec = expectedCostVec)
    return(out)    
}

### Local auxilliary function:
#computeAverageSampleSize <- function(nAnimalVector,intraHerdPrevalence,
#    herdSens, sensitivityDiagTest){
#    ## Number of animals:
#    #####################
#    ## Compute lookup table:
#    nAnimalLookup <- computeOptimalSampleSize(nPopulation = max(nAnimalVector), 
#        prevalence = intraHerdPrevalence, alpha = (1-herdSens),
#        sensitivity = sensitivityDiagTest, specificity = 1, lookupTable = TRUE)
#    nAnimalLookup <- as.data.frame(nAnimalLookup)
#    ## Merge with animal data:
#    nAnimalLookup$interval <- paste("(", nAnimalLookup$N_lower-1, ",",
#        nAnimalLookup$N_upper, "]", sep = "")
#    breaks <- c(nAnimalLookup$N_lower[1]-1, nAnimalLookup$N_upper)     
#    nAnimalTable <- table(nAnimalVector)
#    nAnimalDataFrame <- data.frame(nAnimal = as.numeric(as.character(names(nAnimalTable))),
#        freq = as.vector(nAnimalTable), interval = cut(x = as.numeric(as.character(names(nAnimalTable))), 
#        breaks = breaks))        
#    nAnimalDataFrame <- merge(x = nAnimalDataFrame, 
#        y = subset(nAnimalLookup, select = c("interval", "sampleSize")),
#        by = "interval", all = TRUE)    
#    ## Mean number of animals to be tested per holding:
#    nSampleMean <- sum(with(nAnimalDataFrame, freq*sampleSize))/length(nAnimalVector)
#    return(nSampleMean)
#}
