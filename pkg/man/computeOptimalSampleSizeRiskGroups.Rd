\name{computeOptimalSampleSizeRiskGroups}
\alias{computeOptimalSampleSizeRiskGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FUNCTION to compute the optimal sample size for populations stratified by
risk factors.
}
\description{
Computes the optimal sample size (for each risk group) for a survey to 
substantiate freedom from disease for a population stratified into risk groups. 
The optimal sample size is the smallest sample size that produces 
an alpha-error less than or equal to a prediscribed value for alpha. 
The population is considered as diseased if at least one individual has a 
positive test result. The sample size is computed using a bisection method.
    The sample size can be fixed for a subset of the risk groups via the input 
parameter 'nSampleFixVec' (vector containing sample sizes for the risk groups
with fixed values and NA for the risk groups for which the sample size is to 
be computed). For those risk groups for which the sample size is to be
computed a vector specifying the proportional distribution among the risk 
groups ('nSamplePropVec') needs to be specified.

Example: We have 3 risk groups. For the 2nd risk group we want 20 farms
to be sampled. For the other  risk groups we specify that the sample size 
for risk group 1 should be double the sample size of risk group 3. We
then set:
    nSampleFixVec <- c(NA, 20, NA)
    nSamplePropVec <- c(2,1)
}
\usage{
computeOptimalSampleSizeRiskGroups(nPopulationVec, 
    nRelRiskVec, nSampleFixVec = NULL, nSamplePropVec = NULL, 
    prevalence, alpha = 0.05, sensitivity = 1, 
    specificity = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nPopulationVec}{
Integer vector. Population sizes of the risk groups.
}
  \item{nRelRiskVec}{
Numeric vector. (Relative) infection risks of the 
risk groups.
}
  \item{nSampleFixVec}{
Numeric vector containing NAs (optional argument). 
For risk groups for which the sample size is fixed 
specify the sample size. For the risk groups for which
the sample size should be computed set NA (order of the
risk groups must be the same order as in \code{nPopulationVec}
and \code{nRelRiskVec}) .
}
  \item{nSamplePropVec}{
Numeric vector. For those risk groups for which the 
sample size should be computed a proportional 
distribution of the overall sample size must be specified.
The vector must have the same length as the number of 
NA entries in \code{nSampleFixVec} or if \code{nSampleFixVec} is not 
specified, \code{nSamplePropVec} must have the same length as
\code{nPopulationVec}. 
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
}
\value{
The return value is an integer vector containing the optimal sample size
for every risk group specified in the input variables \code{nPopulationVec}
and \code{nRelRiskVec}. 
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
\code{\link{computePValueRiskGroups}}
}
\examples{
nPopulationVec <- c(500,700)
nRelRiskVec <- c(1,3)
prevalence <- 0.01
alpha <- 0.05
herdSensitivity <- 0.7
specificity <- 1

## Optimal sample size with risk groups: 
nRisk <- computeOptimalSampleSizeRiskGroups(nPopulationVec = 
    nPopulationVec, nRelRiskVec = nRelRiskVec, 
	nSamplePropVec = c(1,4), prevalence = prevalence, 
	alpha = alpha, sensitivity = herdSensitivity, 
	specificity = specificity)
## Optimal sample size without risk groups: 
nNoRisk <- computeOptimalSampleSize(sum(nPopulationVec), 
    prevalence, alpha, herdSensitivity, specificity, FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

