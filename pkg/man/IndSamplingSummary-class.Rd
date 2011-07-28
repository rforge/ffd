\name{IndSamplingSummary-class}
\Rdversion{1.1}
\docType{class}
\alias{IndSamplingSummary-class}
\alias{HTML,IndSamplingSummary-method}
\alias{plot,IndSamplingSummary-method}
\alias{show,IndSamplingSummary-method}
\alias{summary,IndSamplingSummary-method}

\title{Class "IndSamplingSummary"}
\description{
Contains the parameters and the data necessery for a survey
to substantiate freedom from disease 
using "individual sampling". Additionally to the survey
parameters: design prevalence (=prevalence of the disease under
the null hypothesis), overall significance level (=1-confidence),
intra-herd prevalence, sensitivity of the diagnostic test, 
cost per tested animal and cost per tested herd, the object 
contains the the number of herds to 
be tested, the mean overall number of animals to be tested
and the expected costs for a range of possible herd sensitivities.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IndSamplingSummary", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{surveyData}:}{Object of class \code{"SurveyData"}. Containing the 
        necessary survey parameters.}
    \item{\code{herdSensVec}:}{Object of class \code{"numeric"} with 
        values between 0 and 1. Mean herd sensitivity in the population (vector).}
    \item{\code{nHerdsVec}:}{Object of class \code{"numeric"}. Number of herds
        to be tested according to the herd sensitivity \code{herdSensVec} (vector).}
    \item{\code{nHerdsPerRiskGroupMx}:}{Object of class \code{"matrix"}. Number of herds
        to be tested per risk group [columns] and sample limit [rows]
        (if population is stratified by risk groups).}
    \item{\code{nSampleFixVec}:}{Object of class \code{"numeric"}. Numeric vector 
        containing some NAs (optional argument). For risk groups for which the 
        sample size is fixed it specifies the sample size. For the risk groups 
        for which the sample size was computed it was set to NA (order of the
        risk groups is the same as in \code{survey.Data@riskValueData}).}
    \item{\code{probVec}:}{Object of class \code{"numeric"}. Contains the
        sample probabilities for those risk groups for which the sample size 
        was computed (=NA entries in \code{nSampleFixVec}).}
    \item{\code{nAnimalsMeanVec}:}{Object of class \code{"numeric"}. Expected 
        total number of animals to be tested in the survey (vector).}
    \item{\code{expectedCostVec}:}{Object of class \code{"numeric"}. Expected 
        costs of the survey (vector).}
  }
}
\section{Methods}{
  \describe{
    \item{HTML}{\code{signature(x = "IndSamplingSummary")}: Creates an html file containing the 
        summary data and the diagnostic plots. Title, file name, output directory, css-file,
        etc. can additionally be specified using the parameters, \code{filename}, \code{outdir},
        \code{CSSFile}, \code{Title}, as well as all the other parameters of 
        the R2HTML-function \code{HTMLInitFile}.}
    \item{plot}{\code{signature(x = "IndSamplingSummary")}: Create plots of 1) the mean number of animals
        to be tested per herd, 2) the number of herds to be tested, 3) the expected total number of 
        animals to be tested, 4) the expected total cost of the survey plotted against the vector 
        of herd sensitivities.}
    \item{show}{\code{signature(object = "IndSamplingSummary")}: Display structure of the class and content 
        of the slots.}
    \item{summary}{\code{signature(object = "IndSamplingSummary")}: Display structure of the class and a 
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
showClass("IndSamplingSummary")
## Create an object:
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 15, costAnimal = 20)
myIndSamplingSummary <- indSamplingSummary(survey.Data = mySurvey, 
    stepSize = 0.02)
## Display results:
summary(myIndSamplingSummary)
plot(myIndSamplingSummary)
## Write results to an html-file:
\dontrun{
target <- HTMLInitFile(getwd(), filename = "ItdSampling")
HTML(myIndSamplingSummary)
HTMLEndFile()}
}
\keyword{classes}
