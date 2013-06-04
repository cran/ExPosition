corePCA <-
function(DATA,M=NULL,W=NULL,decomp.approach='svd',k=0){

	DATA_dims <- dim(DATA)
#DATA comes in already scaled & centered or not. That happens at the PCA, GPCA	& BADA level.
	
	if(is.null(M) || is.null(W)){
		genFlag <- FALSE
	}else{
		genFlag <- TRUE
	}
	
	if(genFlag){
		pdq_results <- genPDQ(M=M,datain=DATA,W=W,decomp.approach=decomp.approach,k=k)
	}else{
		pdq_results <-    PDQ(DATA,decomp.approach=decomp.approach,k=k)
	}

	#vectorize internally to this function.
	if((!is.null(dim(M))) && (length(M) == (nrow(M) * ncol(M)))){
		M <- diag(M)
	}
	if((!is.null(dim(W))) && (length(W) == (nrow(W) * ncol(W)))){
		W <- diag(W)
	}	

	#rows
	fi <- pdq_results$p %*% pdq_results$Dd
	rownames(fi) <- rownames(DATA)		
	di <- rowSums(fi^2)
	ri <- repmat((1/di),1,pdq_results$ng) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)	
	if(genFlag){
#		if(is.null(dim(M)) && (!is.null(length(M)))){
			ci <- repmat(M,1,pdq_results$ng) * (fi^2)/repmat(t(pdq_results$Dv^2),DATA_dims[1],1)
#		}else{
#			ci <- repmat(diag(M),1,pdq_results$ng) * (fi^2)/repmat(t(pdq_results$Dv^2),DATA_dims[1],1)
#		}
	}else{
		ci <- (fi^2)/repmat(t(pdq_results$Dv^2),DATA_dims[1],1)
	}
	di <- as.matrix(di)		

	#columns
	fj <- pdq_results$q %*% pdq_results$Dd
	rownames(fj) <- colnames(DATA)		
	dj <- rowSums(fj^2)
	rj <- repmat((1/dj),1,pdq_results$ng) * (fj^2)
	rj <- replace(rj,is.nan(rj),0)
	if(genFlag){
#		if(is.null(dim(W)) && (!is.null(length(W)))){
			cj <- repmat(W,1,pdq_results$ng) * (fj^2)/repmat(t(pdq_results$Dv^2),DATA_dims[2],1)
#		}else{
#			cj <- repmat(diag(W),1,pdq_results$ng) * (fj^2)/repmat(t(pdq_results$Dv^2),DATA_dims[2],1)
#		}
	}else{
		cj <- (fj^2)/repmat(t(pdq_results$Dv^2),DATA_dims[2],1)
	}
	dj <- as.matrix(dj)	

	#I can append the masses & weights if necessary in the appropriate functions
	res <- list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=pdq_results$tau,eigs=pdq_results$Dv^2,pdq=pdq_results,X=DATA)
}
