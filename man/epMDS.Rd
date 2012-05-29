\name{epMDS}
\alias{epMDS}
\title{
epMDS: Multidimensional Scaling (MDS) via ExPosition. 
}
\description{
Multidimensional Scaling (MDS) via ExPosition. 
}
\usage{
epMDS(DATA, DATA_is_dist = TRUE, method="euclidean", DESIGN = NULL, make_design_nominal = TRUE, masses = NULL, graphs = TRUE, k = 0)
}
\arguments{
  \item{DATA}{original data to perform a MDS on.}
  \item{DATA_is_dist}{a boolean. If TRUE (default) the DATA matrix should be a symmetric distance matrix. If FALSE, a Euclidean distance of row items will be computed and used.}
  \item{method}{which distance metric should be used (in function \code{\link{dist}}. Default is "euclidean".)}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{masses}{a diagonal matrix that contains the masses (for the row items).}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
}
\details{
	\code{epMDS} performs metric multi-dimensional scaling. Essentially, a PCA for a symmetric distance matrix. 
}
\value{
	See \code{\link{corePCA}} for details on what is returned. epMDS only returns values related to row items (e.g., fi, ci); no column data is returned.
	\item{D}{the distance matrix that was decomposed.}
}
\references{
Abdi, H. (2007). Metric multidimensional scaling. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand Oaks (CA): Sage. pp. 598-605. \cr
O'Toole, A. J., Jiang, F., Abdi, H., and Haxby, J. V. (2005). Partially distributed representations of objects and faces in ventral temporal cortex. \emph{Journal of Cognitive Neuroscience}, \emph{17}(4), 580-590. 
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{corePCA}}, \code{\link{epPCA}}, \code{\link{epGPCA}}
}
\examples{	
	data(jocn.2005.fmri)
	#by default, components 1 and 2 will be plotted.
	mds.res.images <- epMDS(jocn.2005.fmri$images$data)
	#now a look at components 2 and 3.
	mds.res.images$Plotting.Data <- epGraphs(mds.res.images$ExPosition.Data,epPlotInfo=mds.res.images$Plotting.Data,x_axis=2,y_axis=3)
}
\keyword{ multivariate }
