\name{computeOptimalSampleSize}
\alias{computeOptimalSampleSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the optimal sample size.
}
\description{
Computes the optimal sample size for a survey to substantiate
freedom from disease. The optimal sample size is the smallest
sample size that produces an alpha-error less than or equal 
to a prediscribed value \code{alpha}. The population is considered 
as diseased if at least one individual has a positive test result.
The sample size is computed using a bisection method.
The function either computes the sample size for a fixed population
(\code{lookupTable == FALSE}) or a lookup table with sampling sizes 
depending on the population size for individual sampling
(\code{lookupTable == TRUE}); see Ziller et al., 2002.
}
\usage{
computeOptimalSampleSize(nPopulation, prevalence, alpha = 0.05, 
    sensitivity = 1, specificity = 1, lookupTable = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nPopulation}{
Integer. Population size if \code{lookupTable == FALSE} or the largest
considered herd size for the lookup table if \code{lookupTable == TRUE} .
}
  \item{prevalence}{
Numeric between 0 and 1. Design prvalence. The number of diseased
is then computed as \code{max(1,nPopulation*prevalence)}.
}
  \item{alpha}{
Numeric between 0 and 1. Alpha-Error (=error of the first kind, 
significance level) of the underlying significance test. Default 
value = 0.05.
}
  \item{sensitivity}{
Numeric between 0 and 1. Sensitivity of the diagnostic (for one-stage 
sampling) or herd test (for two stage sampling). Default value = 1.
}
  \item{specificity}{
Numeric between 0 and 1. Specificity of the diagnostic (for one-stage 
sampling) or herd test (for two stage sampling). Default value = 1.
}
  \item{lookupTable}{
Logical. TRUE if a lookup table of sample sizes for
individual sampling (see, Ziller et al., 2002) should
be produced. FALSE if the sample size is desired
for a fixed population size (default).
}
}
\value{
The return value is either an integer, the minimal sample size that produces
the desired alpha-error if \code{lookupTable == FALSE} or a matrix with 
columns \code{N_lower}, \code{N_upper}, \code{sampleSize} containing
the sample sizes for the different herd sizes \code{N}. 

E.g., \code{N_lower = 17}, \code{N_upper = 21}, \code{sampleSize = 11} means 
that for holdings with 17-21 animals 11 animals need to be tested in order 
to achieve the desired accuracy (=herd sensitivity).
}
\references{
A.R. Cameron and F.C. Baldock, "A new probablility formula
to substantiate freedom from disease", Prev. Vet. Med. 34 (1998), pp. 1-17.

M. Ziller, T. Selhorst, J. Teuffert, M. Kramer and H. Schlueter,
"Analysis of sampling strategies to substantiate freedom from disease in
large areas", Prev. Vet. Med. 52 (2002), pp. 333-343. 
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}

\seealso{
\code{\link{computePValue}}
}
\examples{
## Compute the number of herds to be tested for limited sampling
## with sampleSizeLtd = 7:
#################################################################
data(sheepData)
## Compute the average herd sensitivity:
alphaList <- computeAlphaLimitedSampling(stockSizeVector = 
    sheepData$nSheep, sampleSizeLtd = 7, 
    intraHerdPrevalence = 0.13, diagSensitivity = 0.9, 
    diagSpecificity = 1)
sensHerd <- 1 - alphaList$meanAlpha
## Number of herds to be tested:
nHerds <- computeOptimalSampleSize(nPopulation = dim(sheepData)[1], 
    prevalence = 0.002, alpha = 0.05, sensitivity = sensHerd, 
    specificity = 1)

## Compute the number of animals to be tested for individual 
## sampling:
#################################################################
sampleSizeIndividual <- computeOptimalSampleSize(nPopulation = 300, 
    prevalence = 0.2, alpha = 0.05, sensitivity = 0.97, 
    specificity = 1, lookupTable = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
