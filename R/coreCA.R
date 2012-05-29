coreCA <-
function(DATA,masses=NULL,weights=NULL,hellinger=FALSE,symmetric=TRUE,k=0){

	DATA_dimensions = dim(DATA)
	
	mRP<-makeRowProfiles(DATA,weights=weights,masses=masses,hellinger=hellinger)
	rowCenter <- mRP$rowCenter
	rowProfiles <- mRP$rowProfiles
	deviations <- mRP$deviations
	masses <- mRP$masses
	M <- diag(masses)
	weights <- mRP$weights
	W <- mRP$W	

	X <- deviations
		
	pdq_results <- genPDQ(M=M,deviations,W=W,k=k)
	taus = (pdq_results$Dv^2/sum(pdq_results$Dv^2))*100
	
	#Rows, F
	fi = pdq_results$p %*% pdq_results$Dd
	rownames(fi) <- rownames(DATA)	
	di = rowSums(fi^2)
	ri = repmat((1/di),1,pdq_results$ng) * (fi^2)
	ri<-replace(ri,is.nan(ri),0)	
	ci = repmat(masses,1,pdq_results$ng) * (fi^2)/repmat(t(pdq_results$Dv^2),DATA_dimensions[1],1)
	di = as.matrix(di)

	#Columns, G
	fj = W %*% pdq_results$q %*% pdq_results$Dd	
	rownames(fj) <- colnames(DATA)	
	dj = rowSums(fj^2)
	rj = repmat((1/dj),1,pdq_results$ng) * (fj^2)
	rj<-replace(rj,is.nan(rj),0)		
	if(hellinger){
		#  cj=(fj.^2)./repmat(sum(fj.^2),J,1);
		#colSums(fj^2)
		cj <- (fj^2)/t(repmat(colSums(fj^2),1,nrow(fj)))
#		if(is.null(dim(W)) && (!is.null(length(W)))){
#			#cj <- repmat(W,1,pdq_results$ng) * (fj^2)/repmat(t(pdq_results$Dv^2),ncol(DATA),1)
#		}else{
#			#cj <- repmat(diag(W),1,pdq_results$ng) * (fj^2)/repmat(t(pdq_results$Dv^2),ncol(DATA),1)
#		}
	}else{	
		cj = repmat(rowCenter,1,pdq_results$ng) * (fj^2)/repmat(t(pdq_results$Dv^2),DATA_dimensions[2],1)
	}
	dj = as.matrix(dj)
	if(!symmetric){
		fj = W %*% pdq_results$q
		rownames(fj) <- colnames(DATA)		
	}		
	
	return(list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=taus,eigs=pdq_results$Dv^2,M=M,W=W,pdq=pdq_results,X=X,hellinger=hellinger))
}
