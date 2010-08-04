\name{IndSampling-class}
\Rdversion{1.1}
\docType{class}
\alias{IndSampling-class}
\alias{HTML,IndSampling-method}
\alias{sample,IndSampling-method}
\alias{show,IndSampling-method}
\alias{summary,IndSampling-method}

\title{Class "IndSampling"}
\description{
Contains the parameters and the data necessary for a survey
to substantiate freedom from disease 
using "individual sampling". Additionally to the survey
parameters: design prevalence (=prevalence of the disease under
the null hypothesis), overall significance level (=1-confidence),
intra-herd prevalence, sensitivity of the diagnostic test, 
cost per tested animal and cost per tested herd, the object 
contains the herd sensitivity the number of herds to be tested, the mean overall 
number of animals to be tested, the expected costs, as well as a 
lookup table for the number of animals to test depending on the
herd size.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IndSampling", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{surveyData}:}{Object of class \code{"SurveyData"}. Contains all the necessary data 
        and specifications for the survey.}
    \item{\code{herdSensitivity}:}{Object of class \code{"numeric"} with values between
        0 and 1. Desired herd sensitivity.}
    \item{\code{nHerds}:}{Object of class \code{"numeric"}. Number of herds
        to be tested according to the herd sensitivity \code{herdSensitivity}.}
    \item{\code{nAnimalsMean}:}{Object of class \code{"numeric"}. Expected 
        total number of animals to be tested in the survey.}
    \item{\code{expectedCost}:}{Object of class \code{"numeric"}. Expected 
        costs of the survey.}
    \item{\code{lookupTable}:}{Object of class \code{"matrix"} with columns \code{N_lower},
        \code{N_upper} and \code{sampleSize} containing the number of animals to 
        test for each herd size.}
  }
}
\section{Methods}{
  \describe{
    \item{HTML}{\code{signature(x = "IndSampling")}: Creates an html file containing the 
        summary data and the diagnostic plots. Title, file name, output directory, css-file,
        etc. can additionally be specified using the parameters, \code{filename}, \code{outdir},
        \code{CSSFile}, \code{Title}, as well as all the other parameters of 
        the R2HTML-function \code{HTMLInitFile}.}
    \item{sample}{\code{signature(x = "IndSampling", size = c("fixed", "dynamic"))}: Sample herds 
        using individual sampling. Additionally
        to the argument \code{x} of type \code{IndSampling} the method takes an argument \code{size},
        which is a character string. For \code{size == "fixed"} the fixed number of herds
        given in \code{x@nHerds} is sampled using simple random sampling. For \code{size == "dynamic"} 
        dynamic sampling is used, i.e., based on real-time computation of the a-posteriori alpha-
        error the sample is updated until the a-posteriori alpha-error falls below the predefined
        significance level x@alpha. The return value is a list with two items: \code{indexSample} is
        a vector of indices of the sample corresponding to \code{x@surveyData@nAnimalVec} and 
        \code{aPostAlpha} containing the a-posteriori alpha-error of the sample.}
    \item{show}{\code{signature(object = "IndSampling")}: Display structure of the class and content 
        of the slots.}
    \item{summary}{\code{signature(object = "IndSampling")}: Display structure of the class and a 
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
Objects of the class \code{IndSampling} are create using the constructor
\code{\link{indSampling}}.
}
\examples{
## Show the structure of the class:
showClass("IndSampling")
## Create an object:
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
myIndSampling <- indSampling(survey.Data = mySurvey, herdSensitivity = 0.7)
## Display results:
summary(myIndSampling)
## Write results to an html-file:
\dontrun{
target <- HTMLInitFile(getwd(), filename = "IndSampling")
HTML(myIndSampling)
HTMLEndFile()}
}
\keyword{classes}
