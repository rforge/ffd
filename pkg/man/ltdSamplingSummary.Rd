\name{ltdSamplingSummary}
\alias{ltdSamplingSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for class 'LtdSamplingSummary'.
}
\description{
Creates an object of the class 'LtdSamplingSummary'. For given survey parameters
(passed to the function as an object of the class \code{\linkS4class{SurveyData}})
\code{ltdSamplingSummary()} computes the mean herd sensitivity, the number of herds to test,
the expected total number of animals to test and the expected total cost of a survey
using limited sampling with a given sequence of animal-level sample sizes. This sequence
ranges from 1 to a given upper bound \code{sampleSizeLtdMax}. If no upper bound is
specified the maximal herd size is used.
}
\usage{
ltdSamplingSummary(survey.Data, sampleSizeLtdMax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survey.Data}{
Object of class \code{\linkS4class{SurveyData}}. Created by using
the function \code{\link{surveyData}}.
}
  \item{sampleSizeLtdMax}{
Positive integer. A series of parameters is computed
for a sequence of sample limits. These sample limits
range from 1 to a given upper bound, defined by
\code{sampleSizeLtdMax}. If no upper bound is specified
then the maximal herd size is used.
}
}
\value{
The function returns an object of the class \code{\linkS4class{LtdSamplingSummary}}.
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
See \code{\linkS4class{LtdSamplingSummary}} and \code{\linkS4class{SurveyData}} for additional details.
}
\examples{
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
myLtdSamplingSummary <- ltdSamplingSummary(survey.Data = mySurvey, 
    sampleSizeLtdMax = 30)
}
\keyword{methods}
