\name{computeAlpha}
\alias{computeAlpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the herd-based alpha-errors (= 1 - herd sensitivity).
}
\description{
For a vector of herd sizes the herd-based alpha-errors (= 1-herd sensitivity) are
computed for either limited or individual sampling; see Ziller et al.
}
\usage{
computeAlpha(nAnimalVec, method, sampleSizeLtd, herdSensitivity, 
    intraHerdPrevalence, diagSensitivity, diagSpecificity = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nAnimalVec}{
Integer vector. Stock sizes of the herds.
}
  \item{method}{
Character string. \code{"individual"} for individual sampling
or \code{"limited"} for limited sampling.
}
  \item{sampleSizeLtd}{
Integer. Required only if \code{method == "limited"}. Sample size 
for limited sampling, i.e., for each herd
\code{sampleSizeLtd} animals are tested, or of the herd contains
less than \code{sampleSizeLtd} animals the entire herd is tested.
}
  \item{herdSensitivity}{
Numeric between 0 and 1. Required only if \code{method == "individual"}.
Desired (minimal) hed sensitivity for individual sampling.
The number of animals to test per herd is determined 
according to that value.
}
  \item{intraHerdPrevalence}{
Numeric between 0 and 1. Intra-herd prevalence. The number of diseased
animals per herd is computed as 
\code{max(1,round(intraHerdPrevalence*nAnimalVec))}
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
of the diagnostic test. The default value is 1, i.e., perfect 
specificity, and is recommended.
}
}
\value{
Returns a vector containing the herd-based alpha-errors, where each
entry in the vector corresponds to an entry in the input argument
\code{nAnimalVec}.
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
Is used in the method \code{sample} for classes \code{\linkS4class{IndSampling}}
and \code{\linkS4class{LtdSampling}}.
}
\examples{
data(sheepData)
## Compute the herd sensitivities usinh limited sampling:
alphaVec <- computeAlpha(nAnimalVec = sheepData$nSheep, 
    method = "limited", sampleSizeLtd = 7, 
    intraHerdPrevalence = 0.2, diagSensitivity = 0.9)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
