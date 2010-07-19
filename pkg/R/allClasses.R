###############################################################################
###############################################################################
## Class "SurveyData"
##
## Contains the specifications and the data for a survey
## to substantiate freedom from disease 
## using "individual sampling" or "limited sampling".
##
## Package: FFD
##
## Ian Kopacka
## 2010-07-13
###############################################################################
###############################################################################

## CLASS DEFINITION:
###############################################################################
###############################################################################
setClass(
    Class = "SurveyData", 
    representation = representation(
        nAnimalVec = "numeric",
        populationData = "data.frame",
        designPrevalence = "numeric",
        alpha = "numeric",
        intraHerdPrevalence = "numeric",
        diagSensitivity = "numeric",
        costHerd = "numeric", 
        costAnimal = "numeric"        
        ),
    validity = function(object)
    { 
        if (any(object@nAnimalVec < 0))
        {
            stop ("[SurveyData validation]: Slot 'nAnimalVec' must not contain negative values.")
        }
        if (any((object@nAnimalVec  - as.integer(object@nAnimalVec)) != 0))
        {
            stop ("[SurveyData validation]: Slot 'nAnimalVec' must contain integer values.")
        }
        
        if ((length(object@nAnimalVec)*dim(object@populationData)[1] > 0)&(length(object@nAnimalVec) != dim(object@populationData)[1])){
            stop ("[SurveyData validation]: Columns of the slot 'populationData' must have the same length as slot 'nAnimalVec'.")
        }        
        if ((length(object@designPrevalence) > 0)){ 
            if((object@designPrevalence <= 0)|(object@designPrevalence >= 1))
            {
                stop ("[SurveyData validation]: Slot 'designPrevalence' must contain values in (0,1).")
            }
        }
        if ((length(object@alpha) > 0)){ 
            if((object@alpha <= 0)|(object@alpha >= 1))
            {
                stop ("[SurveyData validation]: Slot 'alpha' must contain values in (0,1).")
            }
        }
        if ((length(object@intraHerdPrevalence) > 0)){ 
            if((object@intraHerdPrevalence <= 0)|(object@intraHerdPrevalence >= 1))
            {
                stop ("[SurveyData validation]: Slot 'intraHerdPrevalence' must contain values in (0,1).")
            }
        }
        if ((length(object@diagSensitivity) > 0)){ 
            if((object@diagSensitivity <= 0)|(object@diagSensitivity >= 1))
            {
                stop ("[SurveyData validation]: Slot 'diagSensitivity' must contain values in (0,1).")
            }   
        }     
        return(TRUE)
    }    
)

## METHODS:
###############################################################################
###############################################################################


#################################################### SHOW:
##########################################################

setMethod("show", signature(object="SurveyData"),
    function(object){
        displayLimit <- 20
        cat("Object of class 'SurveyData':\n")
        cat("Slots:\n")        
        if (length(object@designPrevalence) > 0){
            cat("@designPrevalence:      ", sprintf("%.3f", object@designPrevalence), "\n")
        } else {
            cat("@designPrevalence:       NO DATA\n")
        }
        if (length(object@alpha) > 0){
            cat("@alpha:                 ", sprintf("%.2f", object@alpha), "\n")
        } else {
            cat("@alpha:                  NO DATA\n")
        }
        if (length(object@intraHerdPrevalence) > 0){
            cat("@intraHerdPrevalence:   ", sprintf("%.2f", object@intraHerdPrevalence), "\n")
        } else {
            cat("@intraHerdPrevalence:    NO DATA\n")
        }
        if (length(object@diagSensitivity) > 0){
            cat("@diagSensitivity:       ", sprintf("%.2f", object@diagSensitivity), "\n")
        } else {
            cat("@diagSensitivity:        NO DATA\n")
        }
        if (length(object@costHerd) > 0){
            cat("@costHerd:              ", sprintf("%.2f", object@costHerd), "\n")
        } else {
            cat("@costHerd:               NO DATA\n")
        }
        if (length(object@costAnimal) > 0){
            cat("@costAnimal:            ", sprintf("%.2f", object@costAnimal), "\n")
        } else {
            cat("@costAnimal:             NO DATA\n")
        }       
        cat("@nAnimalVec:")
        if (length(object@nAnimalVec) > 0){
            cat("\n")
            if (length(object@nAnimalVec) > displayLimit){
                show(object@nAnimalVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@nAnimalVec), "displayed)\n")
            } else {
                show(object@nAnimalVec)
            }
        } else {        
            cat("             NO DATA\n")
        } 
        cat("@poplulationData:")
        if (dim(object@populationData)[1] > 0){
            cat("\n")
            if (dim(object@populationData)[1] > displayLimit){
                show(head(object@populationData, n = displayLimit))
                cat("   (only first",displayLimit,"rows of",dim(object@populationData)[1], "displayed)\n")
            } else {
                show(object@populationData)
            }
        } else {        
            cat("        NO DATA\n")
        }         
    }
)

################################################# SUMMARY:
##########################################################

setMethod("summary", signature(object="SurveyData"),
    function(object){
        cat("Survey Parameters:\n")
        cat("------------------\n")          
        if (length(object@designPrevalence) > 0){
            cat("Design Prevalence:              ", sprintf("%.3f", object@designPrevalence), "\n")
        } else {
            cat("Design Prevalence:               NO DATA\n")
        }
        if (length(object@alpha) > 0){
            cat("Significance level:             ", sprintf("%.2f", object@alpha), "\n")
        } else {
            cat("Significance level:              NO DATA\n")
        }
        if (length(object@intraHerdPrevalence) > 0){
            cat("Intra herd prevalence:          ", sprintf("%.2f", object@intraHerdPrevalence), "\n")
        } else {
            cat("Intra herd prevalence:           NO DATA\n")
        }
        if (length(object@diagSensitivity) > 0){
            cat("Sensitivity of diagnostic test: ", sprintf("%.2f", object@diagSensitivity), "\n")
        } else {
            cat("Sensitivity of diagnostic test:  NO DATA\n")
        }
        if (length(object@costHerd) > 0){
            cat("Cost per tested herd:           ", sprintf("%.2f", object@costHerd), "\n")
        }
        if (length(object@costAnimal) > 0){
            cat("Cost per tested animal:         ", sprintf("%.2f", object@costAnimal), "\n")
        }
        cat("\nSurvey Data:\n")
        cat("------------\n")        
        if (length(object@nAnimalVec) > 0){
            cat("Number of herds:            ", length(object@nAnimalVec),"\n")
            cat("Total number of animals:    ", sum(object@nAnimalVec),"\n")
            cat("Number of animals per herd: \n")
            show(summary(object@nAnimalVec))            
        } else {        
            cat(" no animal data.\n")
        }             
        if (dim(object@populationData)[1] > 0){
            cat("Additional population data: \n")
            str(object@populationData)            
        } else {        
            cat("No additional population data.\n")
        }         
    }
)




###############################################################################
###############################################################################
## Class "LtdSampling"
##
## Contains the parameters and the data for a survey
## to substantiate freedom from disease 
## using "limited sampling". Additionally to the survey
## parameters (design prevalence, overall significance,
## intra-herd prevalence, sensitivity of the diagnostic test, 
## cost per tested animal and cost per tested herd) the object 
## contains the mean herd sensitivity, the number of herds to 
## be tested, the mean overall number of animals to be tested
## and the expected costs.
##
## Package: FFD
##
## Ian Kopacka
## 2010-07-13
###############################################################################
###############################################################################

## CLASS DEFINITION:
###############################################################################
###############################################################################
setClass(
    Class = "LtdSampling", 
    representation = representation(
        surveyData = "SurveyData",
        sampleSizeLtd = "numeric",
        meanHerdSensitivity = "numeric",
        nHerds = "numeric",
        nAnimalsMean = "numeric",
        expectedCost = "numeric"     
        ),
    validity = function(object)
    {      
        if (any(object@sampleSizeLtd <= 0)){
            stop ("[LtdSampling validation]: Slot 'sampleSizeLtd' must be positive.")
        }
        if (any((object@sampleSizeLtd - as.integer(object@sampleSizeLtd)) != 0)){
            stop ("[LtdSampling validation]: Slot 'sampleSizeLtd' must be an integer.")
        }        
        if ((length(object@meanHerdSensitivity) > 0)){ 
            if((object@meanHerdSensitivity <= 0)|(object@meanHerdSensitivity >= 1))
            {
                stop ("[LtdSampling validation]: Slot 'meanHerdSensitivity' must contain values in (0,1).")
            }
        }
        if (any(object@nHerds <= 0)){
            stop ("[LtdSampling validation]: Slot 'nHerds' must be positive.")
        }
        if (any((object@nHerds - as.integer(object@nHerds)) != 0)){
            stop ("[LtdSampling validation]: Slot 'nHerds' must be an integer.")
        } 
        if (length(object@nAnimalsMean) > 0){
            if (object@nAnimalsMean <= 0){
                stop ("[LtdSampling validation]: Slot 'nAnimalsMean' must be a positive numeric")
            }
        }
        return(TRUE)
    }    
)

## METHODS:
###############################################################################
###############################################################################


#################################################### SHOW:
##########################################################

setMethod("show", signature(object="LtdSampling"),
    function(object){
        displayLimit <- 20
        cat("Object of class 'LtdSampling':\n")
        cat("Slots:\n")  
        cat("@surveyData:\n")
        cat("----------------------------\n")
        show(object@surveyData)
        cat("----------------------------\n")
              
        if (length(object@sampleSizeLtd) > 0){
            cat("@sampleSizeLtd:         ", sprintf("%d", object@sampleSizeLtd), "\n")
        } else {
            cat("@sampleSizeLtd:          NO DATA\n")
        }
        if (length(object@meanHerdSensitivity) > 0){
            cat("@meanHerdSensitivity:   ", sprintf("%.2f", object@meanHerdSensitivity), "\n")
        } else {
            cat("@meanHerdSensitivity:    NO DATA\n")
        }
        if (length(object@nHerds) > 0){
            cat("@nHerds:                ", sprintf("%d", object@nHerds), "\n")
        } else {
            cat("@nHerds:                 NO DATA\n")
        }
        if (length(object@nAnimalsMean) > 0){
            cat("@nAnimalsMean:          ", sprintf("%.2f", object@nAnimalsMean), "\n")
        } else {
            cat("@nAnimalsMean:           NO DATA\n")
        }
        if (length(object@expectedCost) > 0){
            cat("@expectedCost:          ", sprintf("%.2f", object@expectedCost), "\n")
        } else {
            cat("@expectedCost:           NO DATA\n")
        }        
    }
)

################################################# SUMMARY:
##########################################################

setMethod("summary", signature(object="LtdSampling"),
    function(object){
        displayLimit <- 20
        cat("LIMITED SAMPLING:\n\n")
        summary(object@surveyData)
        cat("\n")
        cat("Sampling strategy:\n")
        cat("------------------\n")
        if (length(object@sampleSizeLtd) > 0){
            cat("Fixed number of animals to test per herd: ", sprintf("%d",object@sampleSizeLtd), "\n")
        } else {
            cat("No data on number of animals to test per herd.\n")
        }
        if (length(object@meanHerdSensitivity) > 0){
            cat("Mean herd sensitivity:                    ", sprintf("%.2f",object@meanHerdSensitivity), "\n")
        } else {
            cat("No data on mean herd sensitivity.\n")
        }
        if (length(object@nHerds) > 0){
            cat("Number of herds to test:                  ", sprintf("%d",object@nHerds), "\n")
        } else {
            cat("No data on number of herds to test.\n")
        }
        if (length(object@nAnimalsMean) > 0){
            cat("Expected total number of animals to test: ", sprintf("%.2f",object@nAnimalsMean), "\n")
        } else {
            cat("No data on expected total number of animals to test.\n")
        }
        if (length(object@expectedCost) > 0){
            cat("Expected total costs of the survey:       ", sprintf("%.2f",object@expectedCost), "\n")
        } else {
            cat("No data on expected total costs of the survey.\n")
        }        
    }
)

#################################################### HTML:
##########################################################

setMethod("HTML", signature(x = "LtdSampling"),
    function(x, file = .HTML.file, append = TRUE,...){ 
        HTML(paste("<h1>Survey to substantiate Freedom From Disease</h1>"))
        HTML(paste("<h2>Limited Sampling</h2>"))
        HTML("<br>")
        ## Survey Parameters:
        HTML("<h3>Survey Parameters:</h3>")  
        surveyDat <- x@surveyData      
        theTable <- matrix(c(as.character(surveyDat@designPrevalence), 
            as.character(surveyDat@alpha), 
            as.character(surveyDat@intraHerdPrevalence), 
            as.character(surveyDat@diagSensitivity), 
            as.character(surveyDat@costHerd), 
            as.character(surveyDat@costAnimal)), ncol = 1)
        indVec <- c(length(surveyDat@designPrevalence)>0, 
            length(surveyDat@alpha)>0, 
            length(surveyDat@intraHerdPrevalence)>0,
            length(surveyDat@diagSensitivity)>0,
            length(surveyDat@costHerd)>0,
            length(surveyDat@costAnimal)>0)
        rownames(theTable) <- c("Design Prevalence ", "Overall Significance (alpha) ",
            "Intra herd prevalence ", "Sensitivity of diagnostic test ",
            "Cost per herd ", "Cost per animal ")[indVec]
        HTML(theTable, align = "left")
        HTML("<br>")
        ## Data description:
        if (length(surveyDat@nAnimalVec) > 0){
            HTML("<h3>Data Description:</h3>")  
            theTable <- matrix(c(length(surveyDat@nAnimalVec), 
                sum(surveyDat@nAnimalVec), 
                min(surveyDat@nAnimalVec),
                median(surveyDat@nAnimalVec),
                max(surveyDat@nAnimalVec)), ncol = 1)
            rownames(theTable) <- c("Number of herds ", 
                "Total number of animals ",
                "Minimal herd size ",
                "Median herd size ",
                "Maximal herd size ")
            HTML(theTable, align = "left")
            HTML("<br>")
        }
        ## Sample Parameters:
        HTML("<h3>Sampling strategy:</h3>")
        theTable <- matrix(c(as.character(x@sampleSizeLtd),
            as.character(round(x@meanHerdSensitivity,2)),
            as.character(x@nHerds),
            as.character(round(x@nAnimalsMean,2)), 
            as.character(round(x@expectedCost,2))), ncol = 1)
        indVec <- c(length(x@sampleSizeLtd)>0, 
            length(x@meanHerdSensitivity)>0, 
            length(x@nHerds)>0,
            length(x@nAnimalsMean)>0,
            length(x@expectedCost)>0)
        rownames(theTable) <- c("Fixed number of animals to test per herd ", 
            "Mean herd sensitivity ",
            "Number of herds to test ", 
            "Expected total number of animals to test ",
            "Expected total costs of the survey ")[indVec]
        HTML(theTable, align = "left")
        HTML("<br>")       
        ## Return value:
        invisible(x) 
})

###############################################################################
###############################################################################
## Class "LtdSamplingSummary"
##
## Contains the parameters and the data for a survey
## to substantiate freedom from disease 
## using "limited sampling". Additionally to the survey
## parameters (design prevalence, overall significance,
## intra-herd prevalence, sensitivity of the diagnostic test, 
## cost per tested animal and cost per tested herd) the object 
## contains the mean herd sensitivity, the number of herds to 
## be tested, the mean overall number of animals to be tested
## and the expected costs for a range of possible sample limits
## (= fixed number of animals to test per herd).
##
## Package: FFD
##
## Ian Kopacka
## 2010-07-13
###############################################################################
###############################################################################
setClass(
    Class = "LtdSamplingSummary", 
    representation = representation(
        surveyData = "SurveyData",
        sampleSizeLtdVec = "numeric",
        meanHerdSensVec = "numeric",
        nHerdsVec = "numeric",
        nAnimalsMeanVec = "numeric",
        expectedCostVec = "numeric"     
        ),
    validity = function(object)
    {      
        if (any(object@sampleSizeLtdVec <= 0)){
            stop ("[LtdSamplingSummary validation]: Slot 'sampleSizeLtdVec' must contain positive values.")
        }
        if (any((object@sampleSizeLtdVec - as.integer(object@sampleSizeLtdVec)) != 0)){
            stop ("[LtdSamplingSummary validation]: Slot 'sampleSizeLtdVec' must contain an integer vector.")
        }               
        if (any((object@meanHerdSensVec <= 0)|(object@meanHerdSensVec >= 1))){
            stop ("[LtdSamplingSummary validation]: Slot 'meanHerdSensVec' must contain values in (0,1).")
        }        
        if (any(object@nHerdsVec <= 0)){
            stop ("[LtdSamplingSummary validation]: Slot 'nHerdsVec' must contain positive values.")
        }
        if (any((object@nHerdsVec - as.integer(object@nHerdsVec)) != 0)){
            stop ("[LtdSamplingSummary validation]: Slot 'nHerdsVec' must contain an integer vector.")
        } 
        if (any(object@nAnimalsMeanVec <= 0)){
            stop ("[LtdSamplingSummary validation]: Slot 'nAnimalsMeanVec' must contain positive values")
        }        
        return(TRUE)
    }    
)

## METHODS:
###############################################################################
###############################################################################


#################################################### SHOW:
##########################################################

setMethod("show", signature(object="LtdSamplingSummary"),
    function(object){
        displayLimit <- 20
        cat("Object of class 'LtdSamplingSummary':\n")
        cat("Slots:\n")  
        cat("@surveyData:\n")
        cat("----------------------------\n")
        show(object@surveyData)
        cat("----------------------------\n")
         
        cat("@sampleSizeLtdVec:")
        if (length(object@sampleSizeLtdVec) > 0){
            cat("\n")
            if (length(object@sampleSizeLtdVec) > displayLimit){
                show(object@sampleSizeLtdVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@sampleSizeLtdVec), "displayed)\n")
            } else {
                show(object@sampleSizeLtdVec)
            }
        } else {        
            cat("       NO DATA\n")
        }
        cat("@meanHerdSensVec:")
        if (length(object@meanHerdSensVec) > 0){
            cat("\n")
            if (length(object@meanHerdSensVec) > displayLimit){
                show(object@meanHerdSensVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@meanHerdSensVec), "displayed)\n")
            } else {
                show(object@meanHerdSensVec)
            }
        } else {        
            cat("        NO DATA\n")
        }
        cat("@nHerdsVec:")
        if (length(object@nHerdsVec) > 0){
            cat("\n")
            if (length(object@nHerdsVec) > displayLimit){
                show(object@nHerdsVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@nHerdsVec), "displayed)\n")
            } else {
                show(object@nHerdsVec)
            }
        } else {        
            cat("              NO DATA\n")
        }
        cat("@nAnimalsMeanVec:")
        if (length(object@nAnimalsMeanVec) > 0){
            cat("\n")
            if (length(object@nAnimalsMeanVec) > displayLimit){
                show(object@nAnimalsMeanVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@nAnimalsMeanVec), "displayed)\n")
            } else {
                show(object@nAnimalsMeanVec)
            }
        } else {        
            cat("        NO DATA\n")
        }
        cat("@expectedCostVec:")
        if (length(object@expectedCostVec) > 0){
            cat("\n")
            if (length(object@expectedCostVec) > displayLimit){
                show(object@expectedCostVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@expectedCostVec), "displayed)\n")
            } else {
                show(object@expectedCostVec)
            }
        } else {        
            cat("        NO DATA\n")
        }               
    }
)

################################################# SUMMARY:
##########################################################

setMethod("summary", signature(object="LtdSamplingSummary"),
    function(object){
        displayLimit <- 20
        cat("LIMITED SAMPLING:\n\n")
        summary(object@surveyData)
        cat("\n")
        cat("Cost optimal sampling strategy:\n")
        cat("-------------------------------\n")
        if(length(object@expectedCostVec) > 0){
            indCostOpt <- which.min(object@expectedCostVec)
            cat("Fixed number of animals to test per herd: ", sprintf("%d",object@sampleSizeLtdVec[indCostOpt]), "\n")
            cat("Mean herd sensitivity:                    ", sprintf("%.2f",object@meanHerdSensVec[indCostOpt]), "\n")
            cat("Number of herds to test:                  ", sprintf("%d",object@nHerdsVec[indCostOpt]), "\n")
            cat("Expected total number of animals to test: ", sprintf("%.2f",object@nAnimalsMeanVec[indCostOpt]), "\n")
            cat("Expected total costs of the survey:       ", sprintf("%.2f",object@expectedCostVec[indCostOpt]), "\n")
        } else {
            cat("No data regarding costs.\n")
        }        
    }
)

#################################################### HTML:
##########################################################

setMethod("HTML", signature(x = "LtdSamplingSummary"),
    function(x, file = .HTML.file, append = TRUE,...){ 
        HTML(paste("<h1>Survey to substantiate Freedom From Disease</h1>"))
        HTML(paste("<h2>Limited Sampling</h2>"))
        HTML("<br>")
        ## Survey Parameters:
        HTML("<h3>Survey Parameters:</h3>")  
        surveyDat <- x@surveyData      
        theTable <- matrix(c(as.character(surveyDat@designPrevalence), 
            as.character(surveyDat@alpha), 
            as.character(surveyDat@intraHerdPrevalence), 
            as.character(surveyDat@diagSensitivity), 
            as.character(surveyDat@costHerd), 
            as.character(surveyDat@costAnimal)), ncol = 1)
        indVec <- c(length(surveyDat@designPrevalence)>0, 
            length(surveyDat@alpha)>0, 
            length(surveyDat@intraHerdPrevalence)>0,
            length(surveyDat@diagSensitivity)>0,
            length(surveyDat@costHerd)>0,
            length(surveyDat@costAnimal)>0)
        rownames(theTable) <- c("Design Prevalence ", "Overall Significance (alpha) ",
            "Intra herd prevalence ", "Sensitivity of diagnostic test ",
            "Cost per herd ", "Cost per animal ")[indVec]
        HTML(theTable, align = "left")
        HTML("<br>")
        ## Data description:
        if (length(surveyDat@nAnimalVec) > 0){
            HTML("<h3>Data Description:</h3>")  
            theTable <- matrix(c(length(surveyDat@nAnimalVec), 
                sum(surveyDat@nAnimalVec), 
                min(surveyDat@nAnimalVec),
                median(surveyDat@nAnimalVec),
                max(surveyDat@nAnimalVec)), ncol = 1)
            rownames(theTable) <- c("Number of herds ", 
                "Total number of animals ",
                "Minimal herd size ",
                "Median herd size ",
                "Maximal herd size ")
            HTML(theTable, align = "left")
            HTML("<br>")
        }
        ## Sample Parameters:
        HTML("<h3>Cost optimal sampling strategy:</h3>")
        if(length(x@expectedCostVec) > 0){
            indCostOpt <- which.min(x@expectedCostVec)
            theTable <- matrix(c(as.character(x@sampleSizeLtdVec[indCostOpt]),
                as.character(round(x@meanHerdSensVec[indCostOpt],2)),
                as.character(x@nHerdsVec[indCostOpt]),
                as.character(round(x@nAnimalsMeanVec[indCostOpt],2)), 
                as.character(round(x@expectedCostVec[indCostOpt],2))), ncol = 1)
            indVec <- c(length(x@sampleSizeLtdVec)>0, 
                length(x@meanHerdSensVec)>0, 
                length(x@nHerdsVec)>0,
                length(x@nAnimalsMeanVec)>0,
                length(x@expectedCostVec)>0)
            rownames(theTable) <- c("Fixed number of animals to test per herd ", 
                "Mean herd sensitivity ",
                "Number of herds to test ", 
                "Expected total number of animals to test ",
                "Expected total costs of the survey ")[indVec]
            HTML(theTable, align = "left")
            HTML("<br>")        
        } else {
            HTML("No data regarding costs.")
        }        
        ## Return value:
        invisible(x) 
})


#################################################### PLOT:
##########################################################

setMethod("plot", signature(x = "LtdSamplingSummary"),
    function(x,y,...){
        survey.Data <- x@surveyData
        if(length(survey.Data@nAnimalVec > 0)){        
            par(mfrow = c(2, 2))      
           # ## Distribution of the herd sizes:
#            hist(survey.Data@nAnimalVec, xlab = "Herd size", ylab = "Frequency",
#                main = "", col = "#DBA400")               
            ## Mean herd sensitivity:
            plot(x@sampleSizeLtdVec, x@meanHerdSensVec, type = "l",
                xlab = "Sample limit", ylab = "Mean herd sensitivity")
            ## Number of herds to be tested:
            plot(x@sampleSizeLtdVec, x@nHerdsVec, type = "l",
                xlab = "Sample limit", ylab = "No. of herds to be tested")
            ## Total number of animals to be tested:
            plot(x@sampleSizeLtdVec, x@nAnimalsMeanVec, type = "l",
                xlab = "Sample limit", ylab = "Expected total no. of animals to be tested")
            ## Expected cost:
            plot(x@sampleSizeLtdVec, x@expectedCostVec, type = "l",
                xlab = "Sample limit", ylab = "Expected cost")                        
        } else {
            cat("Object of class 'LtdSamplingSummary' contains no data.\n")
        }
    }
)


###############################################################################
###############################################################################
## Class "IndSampling"
##
## Contains the parameters and the data for a survey
## to substantiate freedom from disease 
## using "individual sampling". Additionally to the survey
## parameters (design prevalence, overall significance,
## intra-herd prevalence, sensitivity of the diagnostic test, 
## cost per tested animal and cost per tested herd) the object 
## contains the number of herds to be tested, the mean overall number of 
## animals to be tested, the expected costs and a lookup table containing the
## number of animals to test depending on the herd size.
##
## Package: FFD
##
## Ian Kopacka
## 2010-07-16
###############################################################################
###############################################################################

## CLASS DEFINITION:
###############################################################################
###############################################################################
setClass(
    Class = "IndSampling", 
    representation = representation(
        surveyData = "SurveyData",
        herdSensitivity = "numeric",
        nHerds = "numeric",
        nAnimalsMean = "numeric",
        expectedCost = "numeric",
        lookupTable = "matrix"     
        ),
    validity = function(object)
    {      
        if (any((object@herdSensitivity <= 0)|(object@herdSensitivity >= 1))){
            stop ("[IndSampling validation]: Slot 'herdSensitivity' must contain values in (0,1).")
        }
        if (any(object@herdSensitivity <= 0)){
            stop ("[IndSampling validation]: Slot 'herdSensitivity' must contain positive values.")
        }
        if (any((object@nHerds - as.integer(object@nHerds)) != 0)){
            stop ("[IndSampling validation]: Slot 'nHerds' must contain an integer vector.")
        }
        if (any(object@nAnimalsMean <= 0)){
            stop ("[IndSampling validation]: Slot 'nAnimalsMean' must contain positive values")
        }
        return(TRUE)
    }    
)

## METHODS:
###############################################################################
###############################################################################


#################################################### SHOW:
##########################################################

setMethod("show", signature(object="IndSampling"),
    function(object){
        displayLimit <- 20
        cat("Object of class 'IndSampling':\n")
        cat("Slots:\n")  
        cat("@surveyData:\n")
        cat("----------------------------\n")
        show(object@surveyData)
        cat("----------------------------\n")
              
        if (length(object@herdSensitivity) > 0){
            cat("@herdSensitivity:       ", sprintf("%.2f", object@herdSensitivity), "\n")
        } else {
            cat("@herdSensitivity:        NO DATA\n")
        }
        if (length(object@nHerds) > 0){
            cat("@nHerds:                ", sprintf("%d", object@nHerds), "\n")
        } else {
            cat("@nHerds:                 NO DATA\n")
        }
        if (length(object@nAnimalsMean) > 0){
            cat("@nAnimalsMean:          ", sprintf("%.2f", object@nAnimalsMean), "\n")
        } else {
            cat("@nAnimalsMean:           NO DATA\n")
        }
        if (length(object@expectedCost) > 0){
            cat("@expectedCost:          ", sprintf("%.2f", object@expectedCost), "\n")
        } else {
            cat("@expectedCost:           NO DATA\n")
        }  
        cat("@lookupTable:")  
        if (dim(object@lookupTable)[1] > 0){
            cat("\n")
            show(object@lookupTable)
        } else {        
            cat("            NO DATA\n")
        }    
    }
)

################################################# SUMMARY:
##########################################################

setMethod("summary", signature(object="IndSampling"),
    function(object){
        displayLimit <- 20
        cat("INDIVIDUAL SAMPLING:\n\n")
        summary(object@surveyData)
        cat("\n")
        cat("Sampling strategy:\n")
        cat("------------------\n")
        if (length(object@herdSensitivity) > 0){
            cat("Herd sensitivity:                         ", sprintf("%.2f",object@herdSensitivity), "\n")
        } else {
            cat("No herd sensitivity specified.\n")
        }
        if (length(object@nHerds) > 0){
            cat("Number of herds to test:                  ", sprintf("%d",object@nHerds), "\n")
        } else {
            cat("No data on number of herds to test.\n")
        }
        if (length(object@nAnimalsMean) > 0){
            cat("Expected total number of animals to test: ", sprintf("%.2f",object@nAnimalsMean), "\n")
        } else {
            cat("No data on expected total number of animals to test.\n")
        }
        if (length(object@expectedCost) > 0){
            cat("Expected total costs of the survey:       ", sprintf("%.2f",object@expectedCost), "\n")
        } else {
            cat("No data on expected total costs of the survey.\n")
        } 
        if (dim(object@lookupTable)[1] > 0){            
            ## Format lookup Table for pretty summary:
            ## Index for herd sizes where entire herd is tested:
            indAll <- range(which(object@lookupTable[,"N_upper"] == object@lookupTable[,"sampleSize"]))
            nRows <- dim(object@lookupTable)[1]
            ## Herd sizes where entire herd is tested:
            tempDf <- data.frame(herdSize = paste(object@lookupTable[indAll[1],"N_lower"], "-", 
                object@lookupTable[indAll[2],"N_upper"]), sampleSize = "entire herd",
                stringsAsFactors = FALSE)
            ## Rest of the herd sizes:
            if (nRows > indAll[2]){
                tempDf2 <- data.frame(herdSize = sapply((indAll[2]+1):nRows, function (ii){
                    N_lower <- object@lookupTable[ii,"N_lower"]
                    N_upper <- object@lookupTable[ii,"N_upper"]
                    if (N_lower == N_upper){
                        out <- as.character(N_upper)
                    } else {
                        out <- paste(N_lower, "-", N_upper)
                    }
                    return(out)
                }), sampleSize = object@lookupTable[(indAll[2]+1):nRows,"sampleSize"],
                stringsAsFactors = FALSE)
            ## Paste the two data frames together:
            tempDf <- rbind(tempDf,tempDf2)
            }
            ## Format strings for displaying:
            herdSizeString <- format(c("Herd size", tempDf$herdSize), justify = "centre")
            sampleSizeString <- format(c("No. of animals to test", tempDf$sampleSize), justify = "centre")
            outString <- paste(herdSizeString, sampleSizeString, sep = "  |  ")
            outString <- c(paste(" ",outString[1], sep = ""), 
                Reduce(function(x,y) paste(x,y,sep = ""),rep("-",nchar(outString[1]))),
                outString[-1])
            outString <- paste(rep(" ", length(outString)), outString, rep("\n", length(outString)))
            cat("Lookup table for the number of animals to test per herd:\n\n")
            cat(outString)
        } else {
            cat("No lookup table specified.\n")
        }             
    }
)

#################################################### HTML:
##########################################################

setMethod("HTML", signature(x = "IndSampling"),
    function(x, file = .HTML.file, append = TRUE,...){ 
        HTML(paste("<h1>Survey to substantiate Freedom From Disease</h1>"))
        HTML(paste("<h2>Individual Sampling</h2>"))
        HTML("<br>")
        ## Survey Parameters:
        HTML("<h3>Survey Parameters:</h3>")  
        surveyDat <- x@surveyData      
        theTable <- matrix(c(as.character(surveyDat@designPrevalence), 
            as.character(surveyDat@alpha), 
            as.character(surveyDat@intraHerdPrevalence), 
            as.character(surveyDat@diagSensitivity), 
            as.character(surveyDat@costHerd), 
            as.character(surveyDat@costAnimal)), ncol = 1)
        indVec <- c(length(surveyDat@designPrevalence)>0, 
            length(surveyDat@alpha)>0, 
            length(surveyDat@intraHerdPrevalence)>0,
            length(surveyDat@diagSensitivity)>0,
            length(surveyDat@costHerd)>0,
            length(surveyDat@costAnimal)>0)
        rownames(theTable) <- c("Design Prevalence ", "Overall Significance (alpha) ",
            "Intra herd prevalence ", "Sensitivity of diagnostic test ",
            "Cost per herd ", "Cost per animal ")[indVec]
        HTML(theTable, align = "left")
        HTML("<br>")
        ## Data description:
        if (length(surveyDat@nAnimalVec) > 0){
            HTML("<h3>Data Description:</h3>")  
            theTable <- matrix(c(length(surveyDat@nAnimalVec), 
                sum(surveyDat@nAnimalVec), 
                min(surveyDat@nAnimalVec),
                median(surveyDat@nAnimalVec),
                max(surveyDat@nAnimalVec)), ncol = 1)
            rownames(theTable) <- c("Number of herds ", 
                "Total number of animals ",
                "Minimal herd size ",
                "Median herd size ",
                "Maximal herd size ")
            HTML(theTable, align = "left")
            HTML("<br>")
        }
        ## Sample Parameters:
        HTML("<h3>Sampling strategy:</h3>")
        theTable <- matrix(c(as.character(round(x@herdSensitivity,2)),
            as.character(x@nHerds),
            as.character(round(x@nAnimalsMean,2)), 
            as.character(round(x@expectedCost,2))), ncol = 1)
        indVec <- c(length(x@herdSensitivity)>0, 
            length(x@nHerds)>0,
            length(x@nAnimalsMean)>0,
            length(x@expectedCost)>0)
        rownames(theTable) <- c("Herd sensitivity ",
            "Number of herds to test ", 
            "Expected total number of animals to test ",
            "Expected total costs of the survey ")[indVec]
        HTML(theTable, align = "left")        
        if (dim(x@lookupTable)[1] > 0){
            HTML("<br>")       
            HTML("<h4>Number of animals to test per herd:</h4>")        
            ## Format lookup Table for pretty summary:
            ## Index for herd sizes where entire herd is tested:
            indAll <- range(which(x@lookupTable[,"N_upper"] == x@lookupTable[,"sampleSize"]))
            nRows <- dim(x@lookupTable)[1]
            ## Herd sizes where entire herd is tested:
            tempDf <- data.frame(herdSize = paste(x@lookupTable[indAll[1],"N_lower"], "-", 
                x@lookupTable[indAll[2],"N_upper"]), sampleSize = "entire herd",
                stringsAsFactors = FALSE)
            ## Rest of the herd sizes:
            if (nRows > indAll[2]){
                tempDf2 <- data.frame(herdSize = sapply((indAll[2]+1):nRows, function (ii){
                    N_lower <- x@lookupTable[ii,"N_lower"]
                    N_upper <- x@lookupTable[ii,"N_upper"]
                    if (N_lower == N_upper){
                        out <- as.character(N_upper)
                    } else {
                        out <- paste(N_lower, "-", N_upper)
                    }
                    return(out)
                    }), sampleSize = x@lookupTable[(indAll[2]+1):nRows,"sampleSize"],
                    stringsAsFactors = FALSE)
                ## Paste the two data frames together:
                tempDf <- rbind(tempDf,tempDf2)            
            }
            names(tempDf) <- c("Herd size"," No. of animals to test")
            HTML(tempDf, align = "left", row.names = FALSE)
        }         
        ## Return value:
        invisible(x) 
})



###############################################################################
###############################################################################
## Class "IndSamplingSummary"
##
## Contains the parameters and the data for a survey
## to substantiate freedom from disease 
## using "individual sampling". Additionally to the survey
## parameters (design prevalence, overall significance,
## intra-herd prevalence, sensitivity of the diagnostic test, 
## cost per tested animal and cost per tested herd) the object 
## contains the number of herds to 
## be tested, the mean overall number of animals to be tested
## and the expected costs for a range of possible herd
## sensitivities.
##
## Package: FFD
##
## Ian Kopacka
## 2010-07-15
###############################################################################
###############################################################################
setClass(
    Class = "IndSamplingSummary", 
    representation = representation(
        surveyData = "SurveyData",
        herdSensVec = "numeric",
        nHerdsVec = "numeric",
        nAnimalsMeanVec = "numeric",
        expectedCostVec = "numeric"     
        ),
    validity = function(object)
    {      
        if (any((object@herdSensVec <= 0)|(object@herdSensVec >= 1))){
            stop ("[IndSamplingSummary validation]: Slot 'herdSensVec' must contain values in (0,1).")
        }
        if (any(object@nHerdsVec <= 0)){
            stop ("[IndSamplingSummary validation]: Slot 'nHerdsVec' must contain positive values.")
        }
        if (any((object@nHerdsVec - as.integer(object@nHerdsVec)) != 0)){
            stop ("[IndSamplingSummary validation]: Slot 'nHerdsVec' must contain an integer vector.")
        }
        if (any(object@nAnimalsMeanVec <= 0)){
            stop ("[IndSamplingSummary validation]: Slot 'nAnimalsMeanVec' must contain positive values")
        }         
        return(TRUE)
    }    
)

## METHODS:
###############################################################################
###############################################################################


#################################################### SHOW:
##########################################################
setMethod("show", signature(object="IndSamplingSummary"),
    function(object){
        displayLimit <- 20
        cat("Object of class 'IndSamplingSummary':\n")
        cat("Slots:\n")  
        cat("@surveyData:\n")
        cat("----------------------------\n")
        show(object@surveyData)
        cat("----------------------------\n")
        cat("@herdSensVec:")
        if (length(object@herdSensVec) > 0){
            cat("\n")
            if (length(object@herdSensVec) > displayLimit){
                show(object@herdSensVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@herdSensVec), "displayed)\n")
            } else {
                show(object@herdSensVec)
            }
        } else {        
            cat("        NO DATA\n")
        }
        cat("@nHerdsVec:")
        if (length(object@nHerdsVec) > 0){
            cat("\n")
            if (length(object@nHerdsVec) > displayLimit){
                show(object@nHerdsVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@nHerdsVec), "displayed)\n")
            } else {
                show(object@nHerdsVec)
            }
        } else {        
            cat("              NO DATA\n")
        }
        cat("@nAnimalsMeanVec:")
        if (length(object@nAnimalsMeanVec) > 0){
            cat("\n")
            if (length(object@nAnimalsMeanVec) > displayLimit){
                show(object@nAnimalsMeanVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@nAnimalsMeanVec), "displayed)\n")
            } else {
                show(object@nAnimalsMeanVec)
            }
        } else {        
            cat("        NO DATA\n")
        }
        cat("@expectedCostVec:")
        if (length(object@expectedCostVec) > 0){
            cat("\n")
            if (length(object@expectedCostVec) > displayLimit){
                show(object@expectedCostVec[1:displayLimit])
                cat("   (only first",displayLimit,"elements of",length(object@expectedCostVec), "displayed)\n")
            } else {
                show(object@expectedCostVec)
            }
        } else {        
            cat("        NO DATA\n")
        }                   
    }
)

################################################# SUMMARY:
##########################################################

setMethod("summary", signature(object="IndSamplingSummary"),
    function(object){
        displayLimit <- 20
        cat("INDIVIDUAL SAMPLING:\n\n")
        summary(object@surveyData)
        cat("\n")
        cat("Cost optimal sampling strategy:\n")
        cat("-------------------------------\n")
        if(length(object@expectedCostVec) > 0){
            indCostOpt <- which.min(object@expectedCostVec)
            cat("Herd sensitivity:                         ", sprintf("%.2f",object@herdSensVec[indCostOpt]), "\n")
            cat("Number of herds to test:                  ", sprintf("%d",object@nHerdsVec[indCostOpt]), "\n")
            cat("Expected total number of animals to test: ", sprintf("%.2f",object@nAnimalsMeanVec[indCostOpt]), "\n")
            cat("Expected total costs of the survey:       ", sprintf("%.2f",object@expectedCostVec[indCostOpt]), "\n")            
        } else {
            cat("No data regarding costs.\n")
        }        
    }
)

#################################################### HTML:
##########################################################

setMethod("HTML", signature(x = "IndSamplingSummary"),
    function(x, file = .HTML.file, append = TRUE,...){ 
        HTML(paste("<h1>Survey to substantiate Freedom From Disease</h1>"))
        HTML(paste("<h2>Individual Sampling</h2>"))
        HTML("<br>")
        ## Survey Parameters:
        HTML("<h3>Survey Parameters:</h3>")  
        surveyDat <- x@surveyData      
        theTable <- matrix(c(as.character(surveyDat@designPrevalence), 
            as.character(surveyDat@alpha), 
            as.character(surveyDat@intraHerdPrevalence), 
            as.character(surveyDat@diagSensitivity), 
            as.character(surveyDat@costHerd), 
            as.character(surveyDat@costAnimal)), ncol = 1)
        indVec <- c(length(surveyDat@designPrevalence)>0, 
            length(surveyDat@alpha)>0, 
            length(surveyDat@intraHerdPrevalence)>0,
            length(surveyDat@diagSensitivity)>0,
            length(surveyDat@costHerd)>0,
            length(surveyDat@costAnimal)>0)
        rownames(theTable) <- c("Design Prevalence ", "Overall Significance (alpha) ",
            "Intra herd prevalence ", "Sensitivity of diagnostic test ",
            "Cost per herd ", "Cost per animal ")[indVec]
        HTML(theTable, align = "left")
        HTML("<br>")
        ## Data description:
        if (length(surveyDat@nAnimalVec) > 0){
            HTML("<h3>Data Description:</h3>")  
            theTable <- matrix(c(length(surveyDat@nAnimalVec), 
                sum(surveyDat@nAnimalVec), 
                min(surveyDat@nAnimalVec),
                median(surveyDat@nAnimalVec),
                max(surveyDat@nAnimalVec)), ncol = 1)
            rownames(theTable) <- c("Number of herds ", 
                "Total number of animals ",
                "Minimal herd size ",
                "Median herd size ",
                "Maximal herd size ")
            HTML(theTable, align = "left")
            HTML("<br>")
        }
        ## Sample Parameters:
        HTML("<h3>Cost optimal sampling strategy:</h3>")
        if(length(x@expectedCostVec) > 0){
            indCostOpt <- which.min(x@expectedCostVec)
            theTable <- matrix(c(as.character(x@herdSensVec[indCostOpt]),
                as.character(x@nHerdsVec[indCostOpt]),
                as.character(round(x@nAnimalsMeanVec[indCostOpt],2)), 
                as.character(round(x@expectedCostVec[indCostOpt],2))), ncol = 1)
            indVec <- c(length(x@herdSensVec)>0, 
                length(x@nHerdsVec)>0,
                length(x@nAnimalsMeanVec)>0,
                length(x@expectedCostVec)>0)
            rownames(theTable) <- c("Herd sensitivity ", 
                "Number of herds to test ", 
                "Expected total number of animals to test ",
                "Expected total costs of the survey ")[indVec]            
            HTML(theTable, align = "left")
            HTML("<br>")        
        } else {
            HTML("No data regarding costs.")
        }        
        ## Return value:
        invisible(x) 
})

#################################################### PLOT:
##########################################################

setMethod("plot", signature(x = "IndSamplingSummary"),
    function(x,y,...){
        survey.Data <- x@surveyData
        if(length(survey.Data@nAnimalVec > 0)){        
            par(mfrow = c(2, 2))      
           # ## Distribution of the herd sizes:
#            hist(survey.Data@nAnimalVec, xlab = "Herd size", ylab = "Frequency",
#                main = "", col = "#DBA400")               
            ## Mean number of animals per herd:
            plot(x@herdSensVec, x@nAnimalsMeanVec/x@nHerdsVec, type = "l",
                xlab = "Herd sensitivity", ylab = "Mean no. of animals per herd to be tested")
            ## Number of herds to be tested:
            plot(x@herdSensVec, x@nHerdsVec, type = "l",
                xlab = "Herd sensitivity", ylab = "No. of herds to be tested")
            ## Total number of animals to be tested:
            plot(x@herdSensVec, x@nAnimalsMeanVec, type = "l",
                xlab = "Herd sensitivity", ylab = "Expected total no. of animals to be tested")
            ## Expected cost:
            plot(x@herdSensVec, x@expectedCostVec, type = "l",
                xlab = "Herd sensitivity", ylab = "Expected cost")                        
        } else {
            cat("Object of class 'IndSamplingSummary' contains no data.\n")
        }
    }
)
