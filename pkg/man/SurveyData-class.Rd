\name{SurveyData-class}
\Rdversion{1.1}
\docType{class}
\alias{SurveyData-class}
\alias{show,SurveyData-method}
\alias{summary,SurveyData-method}

\title{Class "SurveyData"}
\description{
Contains the parameters and the data necessery for a survey
to substantiate freedom from disease 
using "individual sampling" or "limited sampling".
The parameters are: design prevalence (=prevalence of the disease under
the null hypothesis), overall significance level (=1-confidence), 
intra-herd prevalence, sensitivity of the diagnostic test, 
as well as cost per tested animal and cost per tested herd. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SurveyData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{    
    \item{\code{nAnimalVec}:}{Object of class \code{"numeric"} (nonnegative). Vector containing the 
        number of animals per holding.}
    \item{\code{populationData}:}{Object of class \code{"data.frame"}. Columns of the data frame
        must have the same length as the vector in slot \code{nAnimalVec}. The data frame can
        contain additional data such as herd id, name and address of the owner etc.}
    \item{\code{designPrevalence}:}{Object of class \code{"numeric"} with values between 0 and 1. 
        Prevalence of the disease under the null hypothesis.}
    \item{\code{alpha}:}{Object of class \code{"numeric"} with values between 0 and 1. 
        Type one error for the statistical test (significance level).}
    \item{\code{intraHerdPrevalence}:}{Object of class \code{"numeric"} with values between 0 and 1. 
        Intra-herd prevalence, i.e., the assumed prevalence of the disease within an infected herd.}
    \item{\code{diagSensitivity}:}{Object of class \code{"numeric"} with values between 0 and 1. 
        Sensitivity of the diagnostic test used.}
    \item{\code{costHerd}:}{Object of class \code{"numeric"}. Cost per tested herd excluding costs for 
        sampling of animals (e.g., travel costs of the vet.)}
    \item{\code{costAnimal}:}{Object of class \code{"numeric"}. Cost per tested animal, e.g., drawing
        of samples + analysis in the lab.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SurveyData")}: Display structure of the class and content 
        of the slots.}
    \item{summary}{\code{signature(object = "SurveyData")}: Display structure of the class and a 
        summary of the content of the slots.}
     }
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}
\note{
No notes added yet.
}

\seealso{
Objects of the class 'SurveyData' can be created using the constructor 
\code{\link{surveyData}} and are used as types for slots in the class
\code{\linkS4class{LtdSampling}}.
}
\examples{
## Show the structure of the class:
showClass("SurveyData")
## Create an object:
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
## Display results:
summary(mySurvey)
}
\keyword{classes}
