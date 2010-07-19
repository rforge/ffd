## Ian Kopacka
## 2010-07-16
##
## Function: indSampling
## 
## Constructor for objects of the class 'IndSampling'.
##
## Package: FFD
##
## Input parameters:
##    survey.Data.......Object of the class 'SurveyData', created by using
##                      the function 'surveyData.R'
##    herdSensitivity...Numeric between 0 and 1. Desired herd sensitivity.
##
## Return value: object of the class 'IndSampling'.

indSampling <- function(survey.Data, herdSensitivity){
    # Error check:
    if ((herdSensitivity >= 1) | (herdSensitivity <= 0)) stop("[indSampling]: herdSensitivity must be a value in (0,1).\n")
    
    ## Number of herds to test:
    nHerds <- computeOptimalSampleSize(nPopulation = length(survey.Data@nAnimalVec), 
        prevalence = survey.Data@designPrevalence, alpha = survey.Data@alpha,
        sensitivity = herdSensitivity, specificity = 1, lookupTable = FALSE)

    ## Lookup table for the number of animals to test per herd depending on the 
    ## herd size and expected total number of animals to test:
    tempList <- computeSampleSizeInd(survey.Data = survey.Data, 
        herdSensitivity = herdSensitivity)
    nAnimalsMean <- tempList$nAnimalsMeanPerHerd*nHerds
    lookupTable <- tempList$lookupTable
    
    ## Expected cost:
    expectedCost <- nHerds*survey.Data@costHerd + nAnimalsMean*survey.Data@costAnimal
    
    ## Create object of class 'IndSampling':
    out <- new("IndSampling", surveyData = survey.Data,
        herdSensitivity = herdSensitivity,
        nHerds = nHerds, 
        nAnimalsMean = nAnimalsMean,
        expectedCost = expectedCost,
        lookupTable = lookupTable)
    return(out)    
}
