\name{computeAlphaLimitedSampling}
\alias{computeAlphaLimitedSampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the average alpha-error (= error of first kind)
for limited sampling.
}
\description{
For sampling strategy "limited sampling" (see Ziller et al., 2002)
the function computes the herd-level alpha-errors (= 1-herd sensitivity) 
for each stock size, as well as the average herd-level
alpha-error.
}
\usage{
computeAlphaLimitedSampling(stockSizeVector, sampleSizeLtd, 
    intraHerdPrevalence, diagSensitivity, 
    diagSpecificity = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stockSizeVector}{
Integer vector. Stock sizes of the herds.
}
  \item{sampleSizeLtd}{
Integer. Sample size for limited sampling, i.e., for each herd
\code{sampleSizeLtd} animals are tested, or of the herd contains
less than \code{sampleSizeLtd} animals the entire herd is tested.
}
  \item{intraHerdPrevalence}{
Numeric between 0 and 1. Intra-herd prevalence. The number of diseased
animals per herd is computed as 
\code{max(1,round(intraHerdPrevalence*stockSizeVector))}
(it is assumed that at least one animal is diseased).
}
  \item{diagSensitivity}{
Numeric between 0 and 1. Sensitivity (= probability of
a testpositive result, given the tested individual is diseased) 
of the diagnostic test.
}
  \item{diagSpecificity}{
Numeric between 0 and 1. Specificity (= probability of
a testnegative result, given the tested individual is not diseased) 
of the diagnostic test.
}
}
\value{
List of two elements:
  \item{alphaDataFrame}{Data frame. Variables \code{size} = vector of the unique 
  herd sizes in the population, \code{alpha} = vector of herd-level alpha errors 
  attained by limited sampling scheme for the different herd sizes.}
  \item{meanAlpha}{Numeric between 0 and 1. Mean alpha-error attained by
  strategy "limited sampling" for given sample size and herd size distribution.}
}
\references{
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
data(sheepData)
alphaList <- computeAlphaLimitedSampling(stockSizeVector = 
    sheepData$nSheep, sampleSizeLtd = 7, 
    intraHerdPrevalence = 0.2, diagSensitivity = 0.9, 
    diagSpecificity = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
