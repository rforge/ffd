\name{lls}
\alias{lls}
\title{
List Objects
}
\description{
Function works like ls() but it returns the names of the objects 
in the workspace, together with class, dimension and size in the
form of a data frame.
}
\usage{
lls(name, pos = -1, envir = as.environment(pos), all.names = FALSE, 
    pattern, classFilter, sort = "size")
}
\arguments{
  \item{name}{
which environment to use in listing the available objects. Defaults to the 
current environment. Although called name for back compatibility, in fact 
this argument can specify the environment in any form; see the details section.
}
  \item{pos}{
an alternative argument to name for specifying the environment as a position in 
the search list. Mostly there for back compatibility.
}
  \item{envir}{
an alternative argument to name for specifying the environment. Mostly there 
for back compatibility.
}
  \item{all.names}{
a logical value. If TRUE, all object names are returned. If 
FALSE, names which begin with a . are omitted.
}
  \item{pattern}{
an optional regular expression. Only names matching pattern are returned. 
glob2rx can be used to convert wildcard patterns to regular expressions.
}
  \item{classFilter}{
optional character string. Only objects of the specified class are returned.
}
  \item{sort}{
optional character string either "size" (default) or "name". Objects are either
sorted by size or alphabetically.
}
}
\details{
The name argument can specify the environment from which object names are 
taken in one of several forms: as an integer (the position in the search list); 
as the character string name of an element in the search list; or as an 
explicit environment (including using sys.frame to access the currently 
active function calls). By default, the environment of the call to lls or 
objects is used. The pos and envir arguments are an alternative way to specify 
an environment, but are primarily there for back compatibility. 
}
\value{
Returns a data frame.
}
\author{
Original author unknown, modified by Ian Kopacka
}
\examples{
lls()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
