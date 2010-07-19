## Ian Kopacka
## 2010-07-16
##
## Function: indSamplingSummary
## 
## Constructor for objects of the class 'IndSamplingSummary'.
## The function computes the number of herds to be tested, 
## the expected total number of animals to be tested and the 
## expected total costs for a sequence of herd sensitivities
## ranging from 0.1 to the sensitivity of the diagnostic test.
## The step size for the herd sensitivities can be specified
## by the user. If no step size is specified a step size of
## 0.02 is used.
##
## Package: FFD
##
## Input parameters:
##    survey.Data...Object of the class 'SurveyData', created by using
##                  the function 'surveyData.R'
##    stepSize......Numeric. A series of parameters is computed for a 
##                  sequence of herd sensitivities. The argument 'stepSize'
##                  specifies the step size used in the discretization of
##                  the herd sensitivities (default = 0.02).
##
## Return value: object of the class 'IndSamplingSummary'.

indSamplingSummary <- function(survey.Data, stepSize = 0.02){
    ## Set value for stepSize:
    herdSensMin <- 0.1
    if (stepSize > (survey.Data@diagSensitivity - herdSensMin)) stop("[indSamplingSummary]: stepSize exceeds range of herd sensitivites.\n")
    if (stepSize <= 0) stop("[indSamplingSummary]: stepSize must be positive.\n")
    herdSensVec <- seq(herdSensMin, survey.Data@diagSensitivity, by = stepSize)    

    out.df <- indSampling.internal(survey.Data = survey.Data, 
        herdSensVec = herdSensVec)
    
    ## Create object of class 'IndSamplingSummary':
    if (any(is.na(out.df$expectedCostVec))) expectedCostVec <- numeric()
    out <- new("IndSamplingSummary", 
        surveyData = survey.Data,
        herdSensVec = out.df$herdSensVec,        
        nHerdsVec = out.df$nHerdsVec,
        nAnimalsMeanVec = out.df$nAnimalsMeanVec,
        expectedCostVec = out.df$expectedCostVec)
    return(out)     
}
