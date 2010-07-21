\name{indSampling}
\alias{indSampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for class 'IndSampling'.
}
\description{
Creates an object of the class 'IndSampling'. For given survey parameters
(passed to the function as an object of the class \code{\linkS4class{SurveyData}})
\code{indSampling()} computes the the number of herds to test,
the expected total number of animals to test, the expected total cost of a survey
using limited sampling with a given herd sensitivity \code{herdSensitivity}, as
well as a lookup table for the number of animals to test per herd, depending on the
herd size.
}
\usage{
indSampling(survey.Data, herdSensitivity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survey.Data}{
Object of class \code{\linkS4class{SurveyData}}. Created by using
the function \code{\link{surveyData}}.
}
  \item{herdSensitivity}{
Numeric between 0 and 1. Desired herd sensitivity.
}
}
\value{
The function returns an object of the class \code{\linkS4class{IndSampling}}.
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
See \code{\linkS4class{IndSampling}} and \code{\linkS4class{SurveyData}} for additional details.
}
\examples{
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
myIndSampling <- indSampling(survey.Data = mySurvey, herdSensitivity = 0.7)
}
\keyword{methods}
