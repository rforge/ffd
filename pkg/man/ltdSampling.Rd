\name{ltdSampling}
\alias{ltdSampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for class 'LtdSampling'.
}
\description{
Creates an object of the class 'LtdSampling'. For given survey parameters
(passed to the function as an object of the class \code{\linkS4class{SurveyData}})
\code{ltdSampling()} computes the mean herd sensitivity, the number of herds to test,
the expected total number of animals to test and the expected total cost of a survey
using limited sampling with a given animal-level sample size \code{sampleSizeLtd}.
}
\usage{
ltdSampling(survey.Data, sampleSizeLtd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survey.Data}{
Object of class \code{\linkS4class{SurveyData}}. Created by using
the function \code{\link{surveyData}}.
}
  \item{sampleSizeLtd}{
Positive integer. Pre-fixed number of animals to be 
tested per holding, irrespective of the herd size 
(if the herd contains fewer animals then the entire
herd needs to be tested).
}
}
\value{
The function returns an object of the class \code{\linkS4class{LtdSampling}}.
}
\references{
A.R. Cameron and F.C. Baldock, "A new probablility formula to substantiate freedom from disease",
Prev. Vet. Med. 34 (1998), pp. 1-17.

A.R. Cameron and F.C. Baldock, "Two-stage sampling surveys to substantiate freedom from disease",
Prev. Vet. Med. 34 (1998), pp. 19-30.

M. Ziller, T. Selhorst, J. Teuffert, M. Kramer and H. Schlueter, "Analysis of sampling strategies to
substantiate freedom from disease in large areas", Prev. Vet. Med. 52 (2002), pp. 333-343.
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}
\seealso{
See \code{\linkS4class{LtdSampling}} and \code{\linkS4class{SurveyData}} for additional details.
}
\examples{
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
myLtdSampling <- ltdSampling(survey.Data = mySurvey, sampleSizeLtd = 7)
}
\keyword{methods}
