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
If values for \code{nSampleFixVec} and/or \code{probVec} are specified, sampling 
if performed with stratification of the population by risk groups.
}
\usage{
ltdSampling(survey.Data, sampleSizeLtd, nSampleFixVec = NULL, 
	probVec = NULL)
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
  \item{nSampleFixVec}{
Numeric vector containing some NAs (optional argument). 
For risk groups for which the sample size is fixed 
specify the sample size. For the risk groups for which
the sample size should be computed set NA (order of the
risk groups must be the same order as in \code{survey.Data@riskValueData}).
}
  \item{probVec}{
Numeric vector. For those risk groups for which the 
sample size should be computed sample probabilities must 
be specified.
The vector must have the same length as the number of 
NA entries in \code{nSampleFixVec} or if \code{nSampleFixVec} is not 
specified, probVec must have the same length as the 
number of rows in \code{survey.Data@riskValueData}.
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
sheepData$size <- ifelse(sheepData$nSheep < 30, "small", "large")
riskValueData <- data.frame(riskGroup = c("small", "large"), 
    riskValues = c(1,2))
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep,
		riskGroupVec = sheepData$size,
		riskValueData = riskValueData,
		populationData = sheepData, designPrevalence = 0.002,
		alpha = 0.05, intraHerdPrevalence = 0.13,
		diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
## Limited sampling without risk groups:    
myLtdSampling <- ltdSampling(survey.Data = mySurvey, sampleSizeLtd = 7)
## Limited sampling with risk groups: 
myLtdSamplingRG <- ltdSampling(survey.Data = mySurvey, sampleSizeLtd = 7, 
	nSampleFixVec = NULL, probVec = c(1,4))
}
\keyword{methods}
