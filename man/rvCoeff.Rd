\name{rvCoeff}
\alias{rvCoeff}
\encoding{UTF-8}
\title{
Perform Rv coefficient computation.
}
\description{
Perform Rv coefficient computation.
}
\usage{
rvCoeff(S, T, type = -1)
}
\arguments{
  \item{S}{A square covariance matrix}
  \item{T}{A square covariance matrix}
  \item{type}{which Rv computation to use. 0 or 1 are faster methods. Any other value (e.g., -1) defaults to a slow, loop-based, method. The loop method is illustrative and not recommended for use.}
}
\value{
	A single value that is the Rv coefficient. 
}
\references{
	Robert, P., & Escoufier, Y. (1976). A Unifying Tool for Linear Multivariate Statistical Methods: The RV-Coefficient. \emph{Journal of the Royal Statistical Society. Series C (Applied Statistics)}, \emph{25}(3), 257--265.}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
