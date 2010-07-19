## Ian Kopacka
## 2010-07-12
##
## Function: ltdSampling.internal
## Package: FFD
## 
## For a fixed Population the number of herds to be tested, the
## expected number of animals to be tested and the costs are 
## computed for a sequence of sampling sizes using "limited
## sampling".
##
## This internal function is called by 'ltdSamplingSummary.R'
##
## Input parameters:
##     survey.Data........Object of the class 'SurveyData', created by using
##                        the function 'surveyData.R'
##     sampleSizeLtdVec...Numeric (integer) vector. Sample limit vector for
##                        which the mean herd sensitivity, the number of
##                        herds to be tested, the expected total number of
##                        animals to be tested and the expected costs are
##                        computed.
##
## Return value: Data frame with columns 'sampleSizeLtdVec', 'meanHerdSensVec', 
##               'nHerdsVec', 'nAnimalsMeanVec' and 'expectedCostVec'.

## Internal auxilliary function:
ltdSampling.internal <- function(survey.Data, sampleSizeLtdVec){
    
    ## Compute the herd sensitivities:
    meanAlphaVec <- sapply(sampleSizeLtdVec, function(x){
        out <- computeAlphaLimitedSampling(stockSizeVector = survey.Data@nAnimalVec,
            sampleSizeLtd = x, intraHerdPrevalence = survey.Data@intraHerdPrevalence,
            diagSensitivity = survey.Data@diagSensitivity,
            diagSpecificity = 1)
        return(out$meanAlpha)
        })
    meanHerdSensVec <- 1 - meanAlphaVec    
    ## Number of herds to be tested according to the
    ## herd sensitivities:
    nHerdsVec <- sapply(meanHerdSensVec, 
        function(x) computeOptimalSampleSize(nPopulation = length(survey.Data@nAnimalVec), 
        prevalence = survey.Data@designPrevalence, alpha = survey.Data@alpha,
        sensitivity = x, specificity = 1, lookupTable = FALSE))
    ## Mean total number of animals to be tested:
    nAnimalsMeanVec <- sapply(sampleSizeLtdVec, function(x) mean(pmin(x,survey.Data@nAnimalVec)))*nHerdsVec
    
    ## Total Cost:
    if ((length(survey.Data@costHerd) > 0) & (length(survey.Data@costAnimal) > 0)){
        expectedCostVec <- nAnimalsMeanVec*survey.Data@costAnimal + 
            nHerdsVec*survey.Data@costHerd
    } else {
        expectedCostVec <- numeric(length(sampleSizeLtdVec))*NA
    }
        
    ## Return value:
    out <- data.frame(sampleSizeLtdVec = sampleSizeLtdVec, 
        meanHerdSensVec = meanHerdSensVec, 
        nHerdsVec = nHerdsVec,
        nAnimalsMeanVec = nAnimalsMeanVec, 
        expectedCostVec = expectedCostVec)
    return(out)    
}
