## Ian Kopacka
## 2010-07-13
##
## Function: ltdSampling
## 
## Constructor for objects of the class 'LtdSampling'.
##
## Package: FFD
##
## Input parameters:
##    survey.Data.....Object of the class 'SurveyData', created by using
##                    the function 'surveyData.R'
##    sampleSizeLtd...Positive integer. Pre-fixed number of animals to be 
##                    tested per holding, irrespective of the herd size 
##                    (if the herd contains fewer animals then the entire
##                    herd needs to be tested).
##
## Return value: object of the class 'LtdSampling'.

ltdSampling <- function(survey.Data, sampleSizeLtd){
    ## Compute the mean herd sensitivity:
    alphaList <- computeAlphaLimitedSampling(stockSizeVector = survey.Data@nAnimalVec, 
        sampleSizeLtd = sampleSizeLtd, 
        intraHerdPrevalence = survey.Data@intraHerdPrevalence, 
        diagSensitivity = survey.Data@diagSensitivity, 
        diagSpecificity = 1)
    ## Compute the number of herds to be tested:
    nHerds <- computeOptimalSampleSize(nPopulation = length(survey.Data@nAnimalVec), 
        prevalence = survey.Data@designPrevalence, 
        alpha = survey.Data@alpha, 
        sensitivity = (1 - alphaList$meanAlpha), 
        specificity = 1, lookupTable = FALSE)
    ## Compute mean overall number of Animals to be tested:
    nAnimalsMean <- nHerds*mean(pmin(survey.Data@nAnimalVec,sampleSizeLtd))
    ## Expected cost of the survey:
    expectedCost <- nHerds*survey.Data@costHerd + nAnimalsMean*survey.Data@costAnimal
    ## Create object of class 'LtdSampling':
    out <- new("LtdSampling", surveyData = survey.Data,
        sampleSizeLtd = sampleSizeLtd,
        meanHerdSensitivity = (1 - alphaList$meanAlpha),
        nHerds = nHerds, nAnimalsMean = nAnimalsMean,
        expectedCost = expectedCost)
    return(out)     
}
