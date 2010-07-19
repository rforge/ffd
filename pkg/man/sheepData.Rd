\name{sheepData}
\alias{sheepData}
\docType{data}
\title{
Simulated test data frame of Austrian sheep holdings.
}
\description{
This test data frame contains a ficticious list of Austrian sheep
holdings with information on the state and the number of
animals.
}
\usage{data(sheepData)}
\format{
  A data frame with 15287 observations on the following 3 variables.
  \describe{
    \item{\code{herdId}}{Numeric vector. Unique identification number of the herd.}
    \item{\code{state}}{Numeric vector. The 9 Austrian states are coded using 
        the numbers 1 to 9.}
    \item{\code{nSheep}}{Numeric vector. Number of animals for each holding.}
  }
}
\source{
Simulated test data.
}
\examples{
data(sheepData)
}
\keyword{datasets}
