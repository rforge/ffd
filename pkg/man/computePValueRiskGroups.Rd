\name{computePValueRiskGroups}
\alias{computePValueRiskGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the probability of finding no testpositives in 
a sample of a certain size for a population stratified into risk groups.
}
\description{
For a population that is stratified into risk groups the function computes the 
probability of finding no testpositives in a sample of given size using an 
imperfect diagnostic test.  For each of the risk groups the population size
\code{nPopulationVec}, the sample size \code{nSampleVec} and the relative 
infection risk \code{nRelRiskVec} must be specified. The discussed probability 
corresponds to the alpha-error (=error of the first kind) of the overall test 
with null hypothesis: prevalence = design prevalence. 
}
\usage{
computePValueRiskGroups(nPopulationVec, nSampleVec, 
    nRelRiskVec, nDiseased, sensitivity, 
    specificity = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nPopulationVec}{
Integer vector. Population sizes of the risk groups.
}
  \item{nSampleVec}{
Integer vector. Sample sizes of the risk groups.
}
  \item{nRelRiskVec}{
Numeric vector. (Relative) infection risks of the 
risk groups.
}
  \item{nDiseased}{
Integer. Number of diseased elements in the population
according to the design prevalence.
}
  \item{sensitivity}{
Numeric between 0 and 1. Sensitivity (= probability of
a testpositive result, given the tested individual is diseased)
of the test (e.g., diagnostic test or herd test).
}
  \item{specificity}{
Numeric between 0 and 1. Specificity (= probability of
a testnegative result, given the tested individual is not 
diseased) of the test (e.g., diagnostic test or herd test).
The default value is 1.
}
}
\value{
The return value is a numeric between 0 and 1. It is
the probability of finding no testpositives (not diseased!) in
the sample.
}
\references{
A.R. Cameron and F.C. Baldock, "A new probablility formula
to substantiate freedom from disease", Prev. Vet. Med. 34 (1998), pp. 1-17.

P.A.J.Martin, A.R. Cameron, M. Greiner, "Demonstrating freedom from disease
using multiple complex data sources. : A new methodology based on scenario
trees", Prev. Vet. Med. 79 (2007), pp. 71 - 97.
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}

\seealso{
Calls \code{\link{computePValue}}
}
\examples{
nPopulationVec <- c(500,700)
nSampleVec <- c(300,200)
nRelRiskVec <- c(1,1)
nDiseased <- round(sum(nPopulationVec)*0.01)
sensitivity <- 0.9
specificity <- 1
alphaError <- computePValue(sum(nPopulationVec), sum(nSampleVec),
	nDiseased, sensitivity, specificity)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

