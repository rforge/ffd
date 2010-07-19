\name{surveyData}
\alias{surveyData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for class 'SurveyData'.
}
\description{
Constructor for objects of the class 'SurveyData'.
}
\usage{
surveyData(nAnimalVec = numeric(), populationData = data.frame(), 
    designPrevalence = numeric(), alpha = numeric(), 
    intraHerdPrevalence = numeric(), diagSensitivity = numeric(), 
    costHerd = numeric(), costAnimal = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nAnimalVec}{
Positive numeric vector containing the number of animals per holding.
}
  \item{populationData}{
Data frame. Columns of the data frame
must have the same length as the vector in \code{nAnimalVec}. The data frame can
contain additional data such as herd id, name and address of the owner etc.
}
  \item{designPrevalence}{
Numeric with  values between 0 and 1. 
Prevalence of the disease under the null hypothesis.
}
  \item{alpha}{
Numeric with values between 0 and 1. 
Type one error for the statistical test (=significance level).
}
  \item{intraHerdPrevalence}{
Numeric with values between 0 and 1. 
Intra-herd prevalence, i.e., the assumed prevalence of the disease within an infected herd.
}
  \item{diagSensitivity}{
Numeric with values between 0 and 1. 
Sensitivity of the diagnostic test used.
}
  \item{costHerd}{
Numeric. Cost per tested herd excluding costs for 
sampling of animals (e.g., travel costs of the vet).
}
  \item{costAnimal}{
Numeric. Cost per tested animal, e.g., drawing
of samples + analysis in the lab.
}
}
\value{
The function returns an object of the class \code{\linkS4class{SurveyData}}.
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}
\seealso{
See \code{\linkS4class{SurveyData}} for additional details on the class.
}
\examples{
data(sheepData)
mySurvey <- surveyData(nAnimalVec = sheepData$nSheep, 
    populationData = sheepData, designPrevalence = 0.002, 
    alpha = 0.05, intraHerdPrevalence = 0.13,
    diagSensitivity = 0.9, costHerd = 30, costAnimal = 7.1)
}
\keyword{methods}
