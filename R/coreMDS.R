coreMDS <-
function(DATA,masses=NULL,decomp.approach='svd',k=0){

	DATA_dims <- dim(DATA)
	#DATA comes in already scaled & centered or not. That happens at the MDS or MDS-extension level.

	if(is.null(masses)){
		masses <- rep(1/DATA_dims[1],DATA_dims[1])
	}
	
	pdq_results <- basePDQ(DATA,is.mds=TRUE,decomp.approach=decomp.approach,k=k)
	
	if((!is.null(dim(masses))) && (length(masses) == (nrow(masses) * ncol(masses)))){
		masses <- diag(masses)
	}
	fi <- matrix(1/sqrt(masses),nrow=length(masses),ncol=length(pdq_results$Dv)) * (pdq_results$p %*% diag(sqrt(pdq_results$Dv)))	
	rownames(fi) <- rownames(DATA)		
	di <- rowSums(fi^2)
	ri <- repmat((1/di),1,pdq_results$ng) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)	
	ci <- repmat(masses,1,pdq_results$ng) * (fi^2)/repmat(t(pdq_results$Dv),DATA_dims[1],1)
	di <- as.matrix(di)		

	#I can append the masses & weights if necessary in the appropriate functions
	res <- list(fi=fi,di=di,ci=ci,ri=ri,masses=masses,t=pdq_results$tau,eigs=pdq_results$Dv,pdq=pdq_results,X=DATA)
}
