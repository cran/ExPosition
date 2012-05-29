\name{designCheck}
\alias{designCheck}
\title{
designCheck
}
\description{
Checks and/or creates a dummy-coded design matrix.
}
\usage{
designCheck(DATA, DESIGN = NULL, make_design_nominal = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{original data that should be matched to a design matrix}
  \item{DESIGN}{a column vector with levels for observations or a dummy-coded matrix}
  \item{make_design_nominal}{a boolean. Will make DESIGN nominal if TRUE (default).}
}
\details{
Returns a properly formatted, dummy-coded (or disjunctive coding) design matrix.
}
\value{
\item{DESIGN}{dummy-coded design matrix}
}
\author{
Derek Beaton
}

\examples{
	#this example is with the default iris set, not the iris set built into ExPosition
	#however, this example shows how I made the iris set for use in ExPosition.
	iris.design <- designCheck(iris[,c(1:4)],DESIGN=as.matrix(iris[,c('Species')]),make_design_nominal=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
