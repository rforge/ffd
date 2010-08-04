\name{LtdSamplingSummary-class}
\Rdversion{1.1}
\docType{class}
\alias{LtdSamplingSummary-class}
\alias{HTML,LtdSamplingSummary-method}
\alias{plot,LtdSamplingSummary-method}
\alias{show,LtdSamplingSummary-method}
\alias{summary,LtdSamplingSummary-method}

\title{Class "LtdSamplingSummary"}
\description{
Contains the parameters and the data necessery for a survey
to substantiate freedom from disease 
using "limited sampling". Additionally to the survey
parameters: design prevalence (=prevalence of the disease under
the null hypothesis), overall significance level (=1-confidence),
intra-herd prevalence, sensitivity of the diagnostic test, 
cost per tested animal and cost per tested herd, the object 
contains the mean herd sensitivity, the number of herds to 
be tested, the mean overall number of animals to be tested
and the expected costs for a range of possible sample limits
(= fixed number of animals to test per herd).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("LtdSamplingSummary", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{surveyData}:}{Object of class \code{"SurveyData"}. Containing the 
        necessary survey parameters.}
    \item{\code{sampleSizeLtdVec}:}{Object of class \code{"numeric"}. Pre-fixed number of 
        animals to be tested per holding, irrespective of the herd size.
        If a herd contains fewer animals the entire herd is tested (vector).}
    \item{\code{meanHerdSensVec}:}{Object of class \code{"numeric"} with 
        values between 0 and 1. Mean herd sensitivity in the population (vector).}
    \item{\code{nHerdsVec}:}{Object of class \code{"numeric"}. Number of herds
        to be tested according to the herd sensitivity \code{meanHerdSensitivity} (vector).}
    \item{\code{nAnimalsMeanVec}:}{Object of class \code{"numeric"}. Expected 
        total number of animals to be tested in the survey (vector).}
    \item{\code{expectedCostVec}:}{Object of class \code{"numeric"}. Expected 
        costs of the survey (vector).}
  }
}
\section{Methods}{
  \describe{
    \item{HTML}{\code{signature(x = "LtdSamplingSummary")}: Creates an html file containing the 
        summary data and the diagnostic plots. Title, file name, output directory, css-file,
        etc. can additionally be specified using the parameters, \code{filename}, \code{outdir},
        \code{CSSFile}, \code{Title}, as well as all the other parameters of 
        the R2HTML-function \code{HTMLInitFile}.}
    \item{plot}{\code{signature(x = "LtdSamplingSummary")}: Create plots of 1) the mean herd sensitivity,
        2) the number of herds to be tested, 3) the expected total number of animals to be tested,
        4) the expected total cost of the survey plotted against the vector of sample limits.}
    \item{show}{\code{signature(object = "LtdSamplingSummary")}: Display structure of the class and content 
        of the slots.}
    \item{summary}{\code{signature(object = "LtdSamplingSummary")}: Display structure of the class and a 
        summary of the content of the slots.}
     }
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}
\note{
No notes yet.
}

\seealso{
The slot \code{surveyData} contains an object of the class 
\code{\linkS4class{SurveyData}}.
}
\examples{
## Show the structure of the class:
showClass("LtdSamplingSummary")
## Create an object:
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
myLtdSamplingSummary <- ltdSamplingSummary(survey.Data = mySurvey, 
    sampleSizeLtdMax = 7)
## Display results:
summary(myLtdSamplingSummary)
plot(myLtdSamplingSummary)
## Write results to an html-file:
\dontrun{
target <- HTMLInitFile(getwd(), filename = "LtdSampling")
HTML(myLtdSamplingSummary)
HTMLEndFile()}
}
\keyword{classes}
