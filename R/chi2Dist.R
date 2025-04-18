#compute the chi2 distance between the rows of a matrix. Send back the distance and the vector of mass	
#this function closely matches the CA preprocessing. But, this will be reserved for use on its own.


#' Chi-square Distance computation
#' 
#' Performs a chi-square distance. Primarily used for \code{\link{epMDS}}.
#' 
#' 
#' @usage chi2Dist(X)
#' @param X Compute chi-square distances between row items.
#' @return \item{D}{Distance matrix for \code{\link{epMDS}} analysis.}
#' \item{MW}{a list of masses and weights. Weights not used in MDS.}
#' @author Hervé Abdi
#' @keywords misc multivariate
#' @export chi2Dist
chi2Dist<-function(X){
    # 1. transform X into Row profiles
    xip = rowSums(X)
    R <- X / xip
    # 2. Masses, weights
    xpp = sum(xip)             # grand total
    m <- xip / xpp             # masses 
    c <- colSums(X) / xpp      # row barycenter
    w = 1/c                    # columns weights
    # Preprocess R
    Rc = t(t(R) - c)           # deviations to barycenter
    Rtilde = t(t(Rc)*sqrt(w))  # weighted R
    S = Rtilde%*%t(Rtilde)     # covariance
    s =diag(S) # diag of
    D = (s - S) + t(s-S)       # Chi2 distance matrix
    return(list(D=D,M=m))
}
