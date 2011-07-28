\name{LtdSampling-class}
\Rdversion{1.1}
\docType{class}
\alias{LtdSampling-class}
\alias{HTML,LtdSampling-method}
\alias{sample,LtdSampling-method}
\alias{show,LtdSampling-method}
\alias{summary,LtdSampling-method}

\title{Class "LtdSampling"}
\description{
Contains the parameters and the data necessary for a survey
to substantiate freedom from disease 
using "limited sampling". Additionally to the survey
parameters: design prevalence (=prevalence of the disease under
the null hypothesis), overall significance level (=1-confidence),
intra-herd prevalence, sensitivity of the diagnostic test, 
cost per tested animal and cost per tested herd, the object 
contains the mean herd sensitivity, the number of herds to 
be tested, the mean overall number of animals to be tested
and the expected costs.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("LtdSampling", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{surveyData}:}{Object of class \code{"SurveyData"}. Contains all the necessary data 
        and specifications for the survey.}
    \item{\code{sampleSizeLtd}:}{Object of class \code{"numeric"}. Pre-fixed number of 
        animals to be tested per holding, irrespective of the herd size.
        If a herd contains fewer animals the entire herd is tested.}
    \item{\code{meanHerdSensitivity}:}{Object of class \code{"numeric"} with 
        values between 0 and 1. Mean herd sensitivity in the population.}
    \item{\code{meanHerdSensPerRG}:}{Object of class \code{"numeric"} 
        with values between 0 and 1. Mean herd sensitivity of each risk group 
        (if population is stratified by risk groups).}
    \item{\code{nHerds}:}{Object of class \code{"numeric"}. Number of herds
        to be tested according to the herd sensitivity \code{meanHerdSensitivity}.}
    \item{\code{nHerdsPerRiskGroup}:}{Object of class \code{"numeric"}. Number of herds
        to be tested per risk group (if population is stratified by risk groups).}
    \item{\code{nSampleFixVec}:}{Object of class \code{"numeric"}. Numeric vector 
        containing some NAs (optional argument). For risk groups for which the 
        sample size is fixed it specifies the sample size. For the risk groups 
        for which the sample size was computed it was set to NA (order of the
        risk groups is the same as in \code{survey.Data@riskValueData}).}
    \item{\code{probVec}:}{Object of class \code{"numeric"}. Contains the
        sample probabilities for those risk groups for which the sample size 
        was computed (=NA entries in \code{nSampleFixVec}).}
    \item{\code{nAnimalsMean}:}{Object of class \code{"numeric"}. Expected 
        total number of animals to be tested in the survey.}
    \item{\code{expectedCost}:}{Object of class \code{"numeric"}. Expected 
        costs of the survey.}
  }
}
\section{Methods}{
  \describe{
    \item{HTML}{\code{signature(x = "LtdSampling")}: Creates an html file containing the 
        summary data and the diagnostic plots. Title, file name, output directory, css-file,
        etc. can additionally be specified using the parameters, \code{filename}, \code{outdir},
        \code{CSSFile}, \code{Title}, as well as all the other parameters of 
        the R2HTML-function \code{HTMLInitFile}.}
    \item{sample}{\code{signature(x = "LtdSampling", size = c("fixed", "dynamic"))}: 
        Sample herds using limited sampling. Additionally
        to the argument \code{x} of type \code{LtdSampling} the method takes an argument \code{size},
        which is a character string. For \code{size == "fixed"} the fixed number of herds
        given in \code{x@nHerds} is sampled using simple random sampling. For \code{size == "dynamic"} 
        dynamic sampling is used, i.e., based on real-time computation of the a-posteriori alpha-
        error the sample is updated until the a-posteriori alpha-error falls below the predefined
        significance level x@alpha. The return value is a list with two items: \code{indexSample} is
        a vector of indices of the sample corresponding to \code{x@surveyData@nAnimalVec} and 
        \code{aPostAlpha} containing the a-posteriori alpha-error of the sample.}
    \item{show}{\code{signature(object = "LtdSampling")}: Display structure of the class and content 
        of the slots.}
    \item{summary}{\code{signature(object = "LtdSampling")}: Display structure of the class and a 
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
\code{\linkS4class{SurveyData}} which is created using \code{\link{surveyData}}. 
Objects of the class \code{LtdSampling} are create using the constructor
\code{\link{ltdSampling}}.
}
\examples{
## Show the structure of the class:
showClass("LtdSampling")
## Create an object:
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
myLtdSampling <- ltdSampling(survey.Data = mySurvey, sampleSizeLtd = 7)
## Display results:
summary(myLtdSampling)
## Write results to an html-file:
\dontrun{
target <- HTMLInitFile(getwd(), filename = "LtdSampling")
HTML(myLtdSampling)
HTMLEndFile()}
}
\keyword{classes}
