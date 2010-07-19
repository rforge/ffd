\name{computeAposterioriError}
\alias{computeAposterioriError}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the a-posteriori error.
}
\description{
For sampling scheme designed to substantiate freedom from
disease the function computes the a-posteriori alpha-error, i.e.,
the actual alpha-error based on the drawn sample. 
}
\usage{
computeAposterioriError(alphaErrorVector, nPopulation, 
    nDiseased, method = "default")
}
\arguments{
  \item{alphaErrorVector}{
Numeric vector. Alpha-error (between 0 and 1) of each herd in the sample.
}
  \item{nPopulation}{
Integer. Population size, i.e., total numer of herds in the population.
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
were implemented in C. Still, for \code{nDiseased} > 7 the computation may take
very long and it is generally not recommended to use the exact calculation. Rather
the approximation should be used for \code{nDiseased} > 7.
}
\value{
The return value is the a-posteriori alpha-error based on the sample at hand 
(numeric scalar).
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}
\examples{
## Freedom from disease using limited sampling with sampleSizeLtd = 7.
## Data: sheep holdings in state "Steiermark".
######################################################################
data(sheepData)
popVec <- sheepData$nSheep[sheepData$state == 6]
N1 <- length(popVec)
sampleSizeLtd <- 7
intraHerdPrev <- 0.13
designPrev <- 0.002
nDiseased <- round(designPrev*N1)
## Draw the sample:
n1 <- 1560
samplePop <- sample(x = popVec, size = n1, replace = FALSE, prob = NULL)
## Compute alpha-errors for the sample:
alphaList <- computeAlphaLimitedSampling(stockSizeVector = samplePop, 
        sampleSizeLtd = sampleSizeLtd, 
        intraHerdPrevalence = intraHerdPrev, 
        diagSensitivity = 0.9, diagSpecificity = 1)
alphaDataFrame <- merge(x = data.frame(size = samplePop), 
    y = alphaList$alphaDataFrame, by = "size", all.x = TRUE, all.y = FALSE)
## Compute the a-posteriori alpha-error:
alphaAPostApprox <- computeAposterioriError(alphaErrorVector = 
    alphaDataFrame$alpha, nPopulation = N1, nDiseased = nDiseased,
    method = "approx")
alphaAPostExact <- computeAposterioriError(alphaErrorVector = 
    alphaDataFrame$alpha, nPopulation = N1, nDiseased = nDiseased,
    method = "exact")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
