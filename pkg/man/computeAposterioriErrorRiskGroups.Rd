\name{computeAposterioriErrorRiskGroups}
\alias{computeAposterioriErrorRiskGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the a-posteriori error considering a population stratified
into risk geroups.
}
\description{
For a sampling scheme designed to substantiate freedom from
disease the function computes the a-posteriori alpha-error, i.e.,
the actual alpha-error based on the drawn sample, when the population is 
stratified into risk groups for which the relative infection risk is known. 
}
\usage{
computeAposterioriErrorRiskGroups(alphaErrorVector, 
    groupVec, groupLevels, nPopulationVec, nRelRiskVec, 
    nDiseased, method = "default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alphaErrorVector}{
Numeric vector. Alpha-error (between 0 and 1) of each herd in the sample.
}
  \item{groupVec}{
Character vector. Risk group to which each of the 
herds in the sample belongs. Must have the same length 
(and order) as \code{alphaErrorVector}.
}
  \item{groupLevels}{
Character vector. (Unique) levels/names of the risk group. Defines the order
of the values in \code{nPopulationVec} and \code{nRelRiskVec}.
}
  \item{nPopulationVec}{
Integer vector. Population sizes of the risk groups. Must have the same length 
(and order) as \code{groupLevels}.
}
  \item{nRelRiskVec}{
Numeric vector. (Relative) infection risks of the risk groups. Must have 
the same length (and order) as \code{groupLevels}.
}
  \item{nDiseased}{
Integer. Number of diseased herds in the population according to the
design prevalence.
}
  \item{method}{
Character string. "exact" for exact error, "approx" for approximation
(recommended for nDiseased > 7).
}
}
\details{
The exact evaluation of the alpha-error is computationally complex, due 
to combinatirical issues. In order to increase effectivity parts of the code 
were implemented in C. Still, for \code{nDiseased} > 3 the computation may take
very long and it is generally not recommended to use the exact calculation. Rather
the approximation should be used for \code{nDiseased} > 3.
}
\value{
The return value is the a-posteriori alpha-error based on the sample at hand 
(numeric scalar).
}

\author{
Ian Kopacka <ian.kopacka@ages.at>
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
## Limited sampling with risk groups:    
myLtdSamplingRG <- ltdSampling(survey.Data = mySurvey, sampleSizeLtd = 20, 
	nSampleFixVec = NULL, probVec = c(1,2))
## Draw sample manually:
set.seed(20110708)
x <- myLtdSamplingRG
indexSampleRG <- sapply(seq(along = x@surveyData@riskValueData$riskGroup), 
	function(ii){
		riskGroup <- as.character(x@surveyData@riskValueData$riskGroup[ii])
		nSampleRG <- x@nHerdsPerRiskGroup[riskGroup]
		indexRG <- which(x@surveyData@riskGroupVec == riskGroup)
		indexOut <- sample(x = indexRG, size = nSampleRG, replace = FALSE)		
	})
indexSample <- sort(Reduce(function(x,y) c(x,y), indexSampleRG))
## Compute the a-posteriori alpha error:
alphaErrorVector <- computeAlpha(nAnimalVec = x@surveyData@nAnimalVec[indexSample], 
     method = "limited", sampleSizeLtd = x@sampleSizeLtd, 
     intraHerdPrevalence = x@surveyData@intraHerdPrevalence, 
     diagSensitivity = x@surveyData@diagSensitivity, diagSpecificity = 1)    
## Determine the number of herds in each risk group:
riskValueDf <- mySurvey@riskValueData[,1:2]
names(riskValueDf) <- c("riskGroup", "riskValues")
riskValueDf$riskGroup <- as.character(riskValueDf$riskGroup)
riskValueDf$id <- seq(along = riskValueDf[,1])
riskGroupTab <- table(mySurvey@riskGroupVec)
riskGroupDf <- data.frame(riskGroup = as.character(names(riskGroupTab)), 
	nPopulation = as.vector(riskGroupTab))
riskValueDf <- merge(x = riskValueDf, y = riskGroupDf, by = "riskGroup",
	sort = FALSE)	
riskValueDf <- riskValueDf[order(riskValueDf$id, decreasing = FALSE),]
aPostAlphaRG <- computeAposterioriErrorRiskGroups(alphaErrorVector = alphaErrorVector, 
     groupVec = x@surveyData@riskGroupVec[indexSample], 
	 groupLevels = riskValueDf$riskGroup,
	 nPopulationVec = riskValueDf$nPopulation,
	 nRelRiskVec = riskValueDf$riskValues,
     nDiseased = max(round(length(x@surveyData@nAnimalVec)*x@surveyData@designPrevalence),1), 
     method = "approx")     
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
