\name{mdsTransform}
\alias{mdsTransform}
\title{
Transform data for MDS analysis.
}
\description{
Transform data for MDS analysis.
}
\usage{
mdsTransform(D, MW)
}
\arguments{
  \item{D}{A distance matrix}
  \item{MW}{A masses and weights list (see \code{computeMW}).}
}
\value{
	\item{S}{a preprocessed matrix that can be decomposed.}
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
