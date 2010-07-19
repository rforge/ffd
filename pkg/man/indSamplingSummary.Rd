\name{indSamplingSummary}
\alias{indSamplingSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for class 'IndSamplingSummary'.
}
\description{
Creates an object of the class 'IndSamplingSummary'. For given survey parameters
(passed to the function as an object of the class \code{\linkS4class{SurveyData}})
\code{indSamplingSummary()} computes the number of herds to test,
the expected total number of animals to test and the expected total cost of a survey
using individual sampling with a given sequence of herd sensitivities. This sequence
ranges from 0.1 to the sensitivity of the diagnostic test specified in \code{survey.Data}.
The step size for the herd sensitivities can be specified by the user via the argument
\code{stepSize}. If no step size is specified a step size of 0.02 is used.
}
\usage{
indSamplingSummary(survey.Data, stepSize = 0.02)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survey.Data}{
Object of class \code{\linkS4class{SurveyData}}. Created by using
the function \code{\link{surveyData}}.
}
  \item{stepSize}{
Numeric. A series of parameters is computed for a 
sequence of herd sensitivities. The argument \code{stepSize}
specifies the step size used in the discretization of
the herd sensitivities (default = 0.02).
}
}
\value{
The function returns an object of the class \code{\linkS4class{IndSamplingSummary}}.
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
See \code{\linkS4class{IndSamplingSummary}} and \code{\linkS4class{SurveyData}} for additional details.
}
\examples{
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 15, costAnimal = 20)
myIndSamplingSummary <- indSamplingSummary(survey.Data = mySurvey, 
    stepSize = 0.02)
}
\keyword{methods}
