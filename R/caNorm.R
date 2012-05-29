caNorm <-
function(X,X_dimensions,colTotal,rowTotal,grandTotal,weights=NULL,masses=NULL){

	rowCenter = colTotal/grandTotal
	if(is.null(masses)){
		masses = rowTotal/grandTotal
	}
	M = diag(masses)
	if(is.null(weights)){
		weights = rowCenter^-1
	}
	W = diag(weights) 
	rowProfiles <- X/((rowSums(X))%*%matrix(1,1,ncol(X)))
	deviations = rowProfiles - (repmat(1,X_dimensions[1],1)%*%rowCenter)
	return(list(rowCenter=rowCenter,masses=masses,M=M,weights=weights,W=W,rowProfiles=rowProfiles,deviations=deviations))
}
