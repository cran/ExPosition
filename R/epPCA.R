epPCA <-
function(DATA,scaleFlag=TRUE,centerFlag=TRUE,DESIGN=NULL,make_design_nominal=TRUE,graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))
	DESIGN <- designCheck(DATA,DESIGN,make_design_nominal)
	DATA <- as.matrix(DATA)
	DATA <- scale(DATA,scale=scaleFlag,center=centerFlag)
	
	res <- corePCA(DATA,k=k)
	
	class(res) <- c("epPCA","list")

	epPlotInfo <- NULL	
	#graphing handled here; also design
	if(graphs){		
		epPlotInfo <- epGraphHandler(res,DATA,DESIGN,main)		
	}

	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
