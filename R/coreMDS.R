#' coreMDS
#' 
#' coreMDS performs metric multidimensional scaling (MDS).
#' 
#' \code{\link{epMDS}} should not be used directly unless you plan on writing
#' extensions to ExPosition. See \code{\link{epMDS}}
#' 
#' @usage coreMDS(DATA, masses = NULL, decomp.approach = 'svd', k = 0)
#' @param DATA original data to decompose and analyze via the singular value
#' decomposition.
#' @param masses a vector or diagonal matrix with masses for the rows
#' (observations). If NULL, one is created.
#' @param decomp.approach string. A switch for different decompositions
#' (typically for speed). See \code{\link{pickSVD}}.
#' @param k number of components to return (this is not a rotation, just an
#' \emph{a priori} selection of how much data should be returned).
#' @return Returns a large list of items which are also returned in
#' \code{\link{epMDS}}.\cr All items with a letter followed by an \emph{i} are
#' for the \emph{I} rows of a DATA matrix. All items with a letter followed by
#' an \emph{j} are for the \emph{J} rows of a DATA matrix.\cr\cr
#' \item{fi}{factor scores for the row items.} \item{di}{square distances of
#' the row items.} \item{ci}{contributions (to the variance) of the row items.}
#' \item{ri}{cosines of the row items.} \item{masses}{a column-vector or
#' diagonal matrix of masses (for the rows)} \item{t}{the percent of explained
#' variance per component (tau).} \item{eigs}{the eigenvalues from the
#' decomposition.} \item{pdq}{the set of left singular vectors (pdq$p) for the
#' rows, singular values (pdq$Dv and pdq$Dd), and the set of right singular
#' vectors (pdq$q) for the columns.} \item{X}{the final matrix that was
#' decomposed (includes scaling, centering, masses, etc...).}
#' @author Derek Beaton and Hervé Abdi.
#' @seealso \code{\link{epMDS}}
#' @references Abdi, H. (2007). Metric multidimensional scaling. In N.J.
#' Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand
#' Oaks (CA): Sage. pp. 598-605. \cr O'Toole, A. J., Jiang, F., Abdi, H., and
#' Haxby, J. V. (2005). Partially distributed representations of objects and
#' faces in ventral temporal cortex. \emph{Journal of Cognitive Neuroscience},
#' \emph{17}(4), 580-590.
#' @keywords multivariate
#' @export coreMDS
coreMDS <-
function(DATA,masses=NULL,decomp.approach='svd',k=0){

	DATA_dims <- dim(DATA)
	#DATA comes in AS A DISTANCE MATRIX. That happens at the MDS or MDS-extension level.

	if(is.null(masses)){
		masses <- rep(1/DATA_dims[1],DATA_dims[1])
	}
	if((!is.null(dim(masses))) && (length(masses) == (nrow(masses) * ncol(masses)))){
		masses <- diag(masses)
	}
		
	S <- mdsTransform(DATA,masses)
	pdq_results <- genPDQ(datain=S,is.mds=TRUE,decomp.approach=decomp.approach,k=k)
	

	fi <- matrix(1/sqrt(masses),nrow=length(masses),ncol=length(pdq_results$Dv)) * 
			(pdq_results$p * matrix(sqrt(pdq_results$Dv),nrow(pdq_results$p),ncol(pdq_results$p),byrow=TRUE))		
	rownames(fi) <- rownames(DATA)		
	di <- rowSums(fi^2)
	ri <- matrix(1/di,nrow(fi),ncol(fi)) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)	
	ci <- matrix(masses,nrow(fi),ncol(fi),byrow=FALSE) * (fi^2)/
		matrix(pdq_results$Dv,nrow(fi),ncol(fi),byrow=TRUE)
	ci <- replace(ci,is.nan(ci),0)	
	di <- as.matrix(di)		

	#I can append the masses & weights if necessary in the appropriate functions
	res <- list(fi=fi,di=di,ci=ci,ri=ri,masses=masses,t=pdq_results$tau,eigs=pdq_results$eigs,pdq=pdq_results,X=S)
}
