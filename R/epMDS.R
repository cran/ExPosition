epMDS <-
function(DATA,DATA_is_dist=TRUE,method="euclidean",DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))	
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	DATA <- as.matrix(DATA)
	DATA_dimensions = dim(DATA)
		
	if(DATA_is_dist && (nrow(DATA)==ncol(DATA))){
		D <- DATA
	}else{
		print('Creating distance matrix from DATA.')
		D <- as.matrix(dist(DATA,method=method,diag=TRUE,upper=TRUE))
	}

	#do this every time.
	MW <- computeMW(D,masses=masses)
	if(is.null(dim(MW$M))){ # it is a vector.
		BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% MW$M)
	}else{
		#BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% diag(masses))
		BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% diag(MW$M))		
	}

	S <- (-(0.5) * BigXi %*% D %*% t(BigXi))
		
	rownames(S) <- rownames(D)
	colnames(S) <- colnames(D)	
	
	res <- corePCA(S,k=k)
	#overwrite res with half of res because it's MDS and we don't care
	res <- list(fi=res$fi,di=res$di,ci=res$ci,ri=res$ri,t=res$t,eigs=res$eigs,pdq=res$pdq,M=MW$M,X=res$X,D=D)
	class(res) <- c("epMDS","list")

	epPlotInfo <- NULL	
	#graphing handled here; also design
	if(graphs){
		epPlotInfo <- epGraphHandler(res,DATA,DESIGN,main)		
	}
	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
