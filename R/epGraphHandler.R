epGraphHandler <-
function(res,DATA,DESIGN,main){
	fi.col <- as.matrix(prettyGraphsColors()[createColorVectorsByDesign(DESIGN)$oc])
	fj.col <- as.matrix(prettyGraphsColors()[createColorVectorsByDesign(matrix(1,ncol(DATA),1))$oc])
	epPlotInfo <- list(fi.col=fi.col,fj.col=fj.col,constraints=NULL)		
	epPlotInfo <- epGraphs(res,main=main,epPlotInfo=epPlotInfo)	
	return(epPlotInfo)
}
