## Ian Kopacka
## 2010-07-14
##
## Function: ltdSamplingSummary
## 
## Constructor for objects of the class 'LtdSamplingSummary'.
## The function computes the mean herd sensitivitiy, the number
## of herds to be tested, the expected total number of animals
## to be tested and the expected total costs for a sequence
## of sample limits (= fixed number of animals to be tested 
## per herd).
##
## Package: FFD
##
## Input parameters:
##    survey.Data........Object of the class 'SurveyData', created by using
##                       the function 'surveyData.R'
##    sampleSizeLtdMax...Positive integer. A series of parameters is computed
##                       for a sequence of sample limits. These sample limits
##                       range from 1 to a given upper bound, defined by
##                       'sampleSizeLtdMax'. If no upper bound is specified
##                       then the maximal herd size is used.
##
## Return value: object of the class 'LtdSamplingSummary'.

ltdSamplingSummary <- function(survey.Data, sampleSizeLtdMax){
    ## Set value for sampleSizeLtdMax:
    if (missing(sampleSizeLtdMax)) sampleSizeLtdMax <- max(survey.Data@nAnimalVec)
    if (sampleSizeLtdMax < 1) stop("[ltdSamplingSummary]: sampleSizeLtdMax must be a positive integer.\n")
    if (sampleSizeLtdMax-as.integer(sampleSizeLtdMax) != 0) stop("[ltdSamplingSummary]: sampleSizeLtdMax must be a positive integer.\n")
    sampleSizeLtdVec <- seq(1,sampleSizeLtdMax)    

    out.df <- ltdSampling.internal(survey.Data = survey.Data, 
        sampleSizeLtdVec = sampleSizeLtdVec)
    
    ## Create object of class 'LtdSamplingSummary':
    if (any(is.na(out.df$expectedCostVec))) expectedCostVec <- numeric()
    out <- new("LtdSamplingSummary", 
        surveyData = survey.Data,
        sampleSizeLtdVec = out.df$sampleSizeLtdVec,
        meanHerdSensVec = out.df$meanHerdSensVec,
        nHerdsVec = out.df$nHerdsVec,
        nAnimalsMeanVec = out.df$nAnimalsMeanVec,
        expectedCostVec = out.df$expectedCostVec)
    return(out)     
}
