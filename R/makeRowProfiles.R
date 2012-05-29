makeRowProfiles <-
function(X,weights=NULL,masses=NULL,hellinger=FALSE){
	
	X_dimensions <- dim(X)
	colTotal <- colSums(X)
	rowTotal <- rowSums(X)
	grandTotal <- sum(X)
	
	if(hellinger){
		hellingerNorm(X,X_dimensions,colTotal,rowTotal,grandTotal,weights,masses)
	}else{
		caNorm(X,X_dimensions,colTotal,rowTotal,grandTotal,weights,masses)
	}

}
