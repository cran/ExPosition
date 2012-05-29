\name{epGraphs}
\alias{epGraphs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
epGraphs: ExPosition plotting function
}
\description{
	ExPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}

\usage{
epGraphs(res, epPlotInfo = NULL, x_axis = 1, y_axis = 2, xlab = NULL, ylab = NULL, main = NULL, contributionPlots = TRUE, correlationPlotter = TRUE, biplots = FALSE)
}

\arguments{
  \item{res}{results from ExPosition (i.e., $ExPosition.Data)}
  \item{epPlotInfo}{results from epGraphs (i.e., $Plotting.Data)}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
  \item{contributionPlots}{a boolean. If TRUE (default), contribution bar plots will be created.}
  \item{correlationPlotter}{a boolean. If TRUE (default), a correlation circle plot will be created. Applies to PCA family of methods (CA is excluded for now).}
  \item{biplots}{a boolean. If FALSE (default), separate plots are made for row items ($fi) and column items ($fj). If TRUE, row ($fi) and column ($fj) items will be on the same plot.}
}
\details{
epGraphs is an interface between \code{\link{ExPosition}} and \code{\link{prettyGraphs}}. 
}
\value{
	The following items are bundled inside of $Plotting.Data:\cr
\item{$fi.col}{the colors that are associated to the row items ($fi).}
\item{$fj.col}{the colors that are associated to the column items ($fj).}
\item{$constraints}{axis constraints for the plots (determines end points of the plots).}
}
\author{
Derek Beaton
}

\seealso{
 \code{\link{prettyGraphs}}
}
\examples{
	#this is for ExPosition's iris data
	data(ep.iris)
	pca.iris.res <- epPCA(ep.iris$data)
	#this will put plotting data into a new variable.
	pca.iris.plotting.data.2.and.3 <- epGraphs(pca.iris.res$ExPosition.Data,x_axis=2,y_axis=3)
	#this will overwrite the data in pca.iris.res$Plotting.Data. Warnings will be provided as to what is overwritten.
	pca.iris.res$Plotting.Data <- epGraphs(pca.iris.res$ExPosition.Data,epPlotInfo=pca.iris.res$Plotting.Data,x_axis=2,y_axis=3)	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ misc }
