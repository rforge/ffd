\name{computePValue}
\alias{computePValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the probability of finding no testpositives in 
a sample of a certain size.
}
\description{
For a population of size \code{nPopulation} with a given design prevalence
the function computes the probability of finding no testpositives
in a sample of size \code{nSample} if an imperfect test is used (given sensitivity
and specificity). This probability corresponds to the alpha-error
(=error of the first kind) of the overall test with null hypothesis:
prevalence = design prevalence. A modified hypergeometric formula
is used; see Cameron, Baldock, 1998.
}
\usage{
computePValue(nPopulation, nSample, nDiseased, 
    sensitivity, specificity = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nPopulation}{
Integer. Population size.
}
  \item{nSample}{
Integer. Size of sample.
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
}
\author{
Ian Kopacka <ian.kopacka@ages.at>
}
\seealso{
\code{\link{computeOptimalSampleSize}}, \code{\link{computeAlphaLimitedSampling}}
}
\examples{
alphaError <- computePValue(nPopulation = 3000, 
    nSample = 1387, nDiseased = 6, sensitivity = 0.85, specificity = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
