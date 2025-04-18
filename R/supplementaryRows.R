#this function works as a shortcut for users. It's a "recognition engine" to auto perform 1) correct preprocessing and 2) supplemental projection.

#RE: PCA -- supplementary measures should always be center/scaled by active variable constraints


#' Supplementary rows
#' 
#' Computes factor scores for supplementary observations (rows).
#' 
#' This function recognizes the class types of: \code{\link{epPCA}},
#' \code{\link{epMDS}}, \code{\link{epCA}}, \code{\link{epMCA}} and
#' \code{TExPosition} methods. Further, the function recognizes if Hellinger
#' (as opposed to row profiles; in CA, MCA and DICA) were used.
#' 
#' @usage supplementaryRows(SUP.DATA, res)
#' @param SUP.DATA a data matrix of supplementary observations (must have the
#' same measures [columns] as active data)
#' @param res ExPosition or TExPosition results
#' @return A list of values containing:\cr \item{fii}{factor scores computed
#' for supplemental observations} \item{dii}{squared distances for supplemental
#' observations} \item{rii}{cosines for supplemental observations}
#' @author Derek Beaton
#' @keywords misc multivariate
#' @export supplementaryRows
supplementaryRows <- function(SUP.DATA,res){
	SUP.DATA <- as.matrix(SUP.DATA)
	
	output.types <- c("expoOutput","texpoOutput")
	data.types <- c("ExPosition.Data","TExPosition.Data")
	mds.types <- c('epMDS')#can add DiSTATIS to this.
	pca.types <- c('epPCA','tepBADA')
	ca.types <- c('epCA','epMCA','tepDICA')	
		
		
	#TEST THIS FURTHER... I SHOULD BE ABLE TO RECOGNZIE TEHSE...	
	if(class(res)[1] %in% output.types){
		indicator <- which(output.types %in% class(res)[1])
		if(any(names(res) %in% data.types) && length(names(res))==2){
			if(output.types[indicator]=="expoOutput"){
				res <- res$ExPosition.Data
			}
			if(output.types[indicator]=="texpoOutput"){
				res <- res$TExPosition.Data
			}
		}else{
			stop(paste("res class type is unknown:",names(res),sep=" "))
		}
	}
		
	if((class(res)[1] %in% c(pca.types))){
		#some trickery happens here... if no res$W is available, it is passed as NULL.
		sup.transform <- pcaSupplementaryRowsPreProcessing(SUP.DATA,center=res$center,scale=res$scale,W=res$W)
		sup.proj <- supplementalProjection(sup.transform,res$fj,res$pdq$Dv)
	}
	
	 else if((class(res)[1] %in% c(ca.types))){
		if(res$hellinger){
			#sup.transform <- hellingerSupplementaryRowsPreProcessing(SUP.DATA,center=res$c)
			sup.transform <- hellingerSupplementaryRowsPreProcessing(SUP.DATA,center=res$c)
			sup.proj <- supplementalProjection(sup.transform,f.scores=res$fj,Dv=res$pdq$Dv,symmetric=res$symmetric)
		}else{
			sup.transform <- caSupplementalElementsPreProcessing(SUP.DATA)
			#else
			if((class(res)[1] %in% c('epMCA'))){ ##stupid corrections.
				sup.proj <- supplementalProjection(sup.transform,res$fj,res$pdq$Dv,scale.factor=res$pdq$Dv/res$pdq.uncor$Dv[1:length(res$pdq$Dv)],symmetric=res$symmetric)
			}else{
				sup.proj <- supplementalProjection(sup.transform,res$fj,res$pdq$Dv,symmetric=res$symmetric)
			}
		}
	}
	
	 else if((class(res)[1] %in% c(mds.types))){
		sup.transform <- mdsSupplementalElementsPreProcessing(SUP.DATA,res$D,res$M)
		sup.proj <- supplementalProjection(sup.transform,res$fi,res$pdq$Dv)
	}else{
		stop("Unknown class type. Supplementary projection computation must stop.")	
	}
	
	fii <- sup.proj$f.out
	dii <- sup.proj$d.out	
	rii <- sup.proj$r.out
	rownames(fii) <- rownames(dii) <- rownames(rii) <- rownames(SUP.DATA)
	return(list(fii=fii,dii=dii,rii=rii))
}
