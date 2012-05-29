epGPCA <-
function(DATA,scaleFlag=TRUE,centerFlag=TRUE,DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,weights=NULL,graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))	
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	DATA <- as.matrix(DATA)
	DATA <- scale(DATA,scale=scaleFlag,center=centerFlag)	
	MW <- computeMW(DATA,masses=masses,weights=weights)
	res <- corePCA(DATA,M=MW$M,W=MW$W,k=k)
	res<-list(fi=res$fi,di=res$di,ci=res$ci,ri=res$ri,fj=res$fj,cj=res$cj,rj=res$rj,dj=res$dj,t=res$t,eigs=res$pdq$Dv^2,M=MW$M,W=MW$W,pdq=res$pdq,X=res$X)
	class(res) <- c("epGPCA","list")
	
	epPlotInfo <- NULL	
	#graphing handled here; also design
	if(graphs){
		
		epPlotInfo <- epGraphHandler(res,DATA,DESIGN,main)		
	}

	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
