epGraphs <-
function(res,epPlotInfo=NULL,x_axis=1,y_axis=2,xlab=NULL,ylab=NULL,main=NULL,contributionPlots=TRUE,correlationPlotter=TRUE,biplots=FALSE){

	if(is.null(main)){
		main <- deparse(substitute(res))
		if(length(unlist(strsplit(main,"")))>40){
			main <- "Results"
		}
	}
	
	#A simple override/check. If someone puts in expoOutput class data, epGraphs will recognize it.
	if(class(res)[1] == "expoOutput"){
		if(length(res)==2){
			epPlotInfo <- res$Plotting.Data
		}
		res <- res$ExPosition.Data
	}	

	pca.types <- c('epPCA','epMDS','epGPCA')
	ca.types <- c('epCA','epMCA')
	#perhaps make this stuff a function, or have TExPosition call all of epGraphs.
	if(!(class(res)[1] %in% c(pca.types,ca.types))){
		stop("Unknown ExPosition class. Plotting has stopped.")
	}else{
		if(is.null(xlab)){
			xlab <- paste("Component ",x_axis," variance: ", round(res$t[x_axis],3), "%",sep="")
		}
		if(is.null(ylab)){
			ylab <- paste("Component ",y_axis," variance: ", round(res$t[y_axis],3), "%",sep="")
		}		
		if(is.null(epPlotInfo)){
			epPlotInfo <- list(fi.col=NULL,fj.col=NULL,constraints=NULL)
		}else{
			#quick checks here for correctness.
			if(class(res)[1]=='epMDS'){
				if(!(nrow(res$fi)==nrow(epPlotInfo$fi.col))){
					print('Dimension mismatch. epPlotInfo will be reset.')
					epPlotInfo$fi.col <- NULL
					epPlotInfo$fj.col <- NULL
					epPlotInfo$constraints <- NULL
				}
			}else{
				if(!(nrow(res$fi)==nrow(epPlotInfo$fi.col)) || !(nrow(res$fj)==nrow(epPlotInfo$fj.col))){
					print('Dimension mismatch. epPlotInfo will be reset.')
					epPlotInfo$fi.col <- NULL
					epPlotInfo$fj.col <- NULL
					epPlotInfo$constraints <- NULL
				}
			}		
		}
		fi.col <- epPlotInfo$fi.col
		fj.col <- epPlotInfo$fj.col
	
		#use the constraints calculator here.
	#	if(!is.null(constraints)){
	#		constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=constraints)
	#	}else if(!is.null(epPlotInfo)){
			constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=epPlotInfo$constraints)
	#	}else{
	#		constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis)	
	#	}		
			
		fi.plot.info <- prettyPlot(res$fi,x_axis=x_axis,y_axis=y_axis,col=fi.col,new_window=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,contributionCircles=TRUE,contributions=res$ci)
		if(!(class(res)[1]=='epMDS')){
			if(biplots){
				fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,new_window=FALSE,contributionCircles=TRUE,contributions=res$cj)
			}else{
				fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,new_window=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,contributionCircles=TRUE,contributions=res$cj)		
			}
		}
		if(contributionPlots){
			contributionBars(res$fi,res$ci,x_axis=x_axis,y_axis=y_axis,main=main,col=fi.plot.info$col)
			if(!(class(res)[1]=='epMDS')){
				contributionBars(res$fj,res$cj,x_axis=x_axis,y_axis=y_axis,main=main,col=fj.plot.info$col)
			}
		}		
		if(correlationPlotter && class(res)[1]%in%pca.types){
			if(class(res)[1]=='epMDS'){
				correlationPlotter(res$X,res$fi,col=fi.plot.info$col,x_axis=1,y_axis=2,xlab=xlab,ylab=ylab,main=main) 
			}else{
				correlationPlotter(res$X,res$fi,col=fj.plot.info$col,x_axis=1,y_axis=2,xlab=xlab,ylab=ylab,main=main) 
			}
		}								
		
	}
	
	if(class(res)[1]=='epMDS'){
		epPlotInfo <- list(fi.col=fi.plot.info$col,fj.col=fi.plot.info$col,constraints=constraints)	
	}else{
		epPlotInfo <- list(fi.col=fi.plot.info$col,fj.col=fj.plot.info$col,constraints=constraints)
	}

	class(epPlotInfo) <- c("epGraphs", "list")
	return(epPlotInfo)	
}
