epGraphs <-
function(res,DESIGN=NULL,x_axis=1,y_axis=2,fi.col=NULL,fi.pch=NULL,fj.col=NULL,fj.pch=NULL,col.offset=NULL,constraints=NULL,xlab=NULL,ylab=NULL,main=NULL,contributionPlots=TRUE,correlationPlotter=TRUE,biplots=FALSE,graphs=TRUE){
	
	
	pca.types <- c('epPCA','epMDS','epGPCA')
	ca.types <- c('epCA','epMCA')	
	
	#A simple override/check. If someone puts in expoOutput class data, epGraphs will recognize it.
	epPlotInfo <- NULL
	if(class(res)[1] == "expoOutput"){
		if(length(res)==2){
			epPlotInfo <- res$Plotting.Data
		}
		res <- res$ExPosition.Data
	}		
			
	#if the override/check fails, it skips to this. Which means it was internal to ep*()
	if(!(class(res)[1] %in% c(pca.types,ca.types))){
		stop("Unknown ExPosition class. Plotting has stopped.")
	}else{
		###Use this block to establish defaults.
		if(is.null(main)){
			main <- deparse(substitute(res))
		}		
		if(length(unlist(strsplit(main,"")))>40){
			main <- "Results"
		}		
		if(is.null(xlab)){
			xlab <- paste("Component ",x_axis," variance: ", round(res$t[x_axis],3), "%",sep="")
		}
		if(is.null(ylab)){
			ylab <- paste("Component ",y_axis," variance: ", round(res$t[y_axis],3), "%",sep="")
		}
		#epPlotInfo check will look for proper colors & constraints, mostly.
		if(!is.null(epPlotInfo)){
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
		}else{
			epPlotInfo <- list(fi.col=NULL,fj.col=NULL,constraints=NULL)	
		}
		
		#fi.col, fj.col, and constraints take precedence over epPlotInfo. This is because epPlotInfo only exists via expoOutput.			
		if(is.null(fi.col) || nrow(fi.col)!=nrow(res$fi)){
			if(is.null(epPlotInfo$fi.col)){
				if(is.null(DESIGN)){
					fi.col <- createColorVectorsByDesign(matrix(1,nrow(res$fi),1),offset=col.offset)$oc
				}else{
					fi.col <- createColorVectorsByDesign(DESIGN,offset=col.offset)$oc
				}
			}else{
				fi.col <- epPlotInfo$fi.col
			}
		}
		
		if(is.null(fi.pch) || nrow(fi.pch)!=nrow(res$fi)){
			if(is.null(epPlotInfo$fi.pch)){
				fi.pch <- fi.pch <- as.matrix(rep(21,nrow(res$fi)))
			}else{
				fi.pch <- epPlotInfo$fi.pch
			}
		}
			
		if(class(res)[1]!='epMDS'){
			if(is.null(fj.col) || nrow(fj.col)!=nrow(res$fj)){
				if(is.null(epPlotInfo$fj.col)){
					fj.col <- createColorVectorsByDesign(matrix(1,nrow(res$fj),1),hsv=FALSE,offset=col.offset)$oc
				}else{
					fj.col <- epPlotInfo$fj.col	
				}
			}
			
			if(is.null(fj.pch) || nrow(fj.pch)!=nrow(res$fj)){
				if(is.null(epPlotInfo$fi.pch)){
					fj.pch <- fj.pch <- as.matrix(rep(21,nrow(res$fj)))
				}else{
					fj.pch <- epPlotInfo$fj.pch
				}
			}			
		}
		
		if(is.null(constraints)){
			if(!is.null(epPlotInfo$constraints)){
				constraints <- epPlotInfo$constraints
			}
			#this is needed because if we switch axes, it could be different constraints.
			constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=constraints)			
		}
		#by the time I get here, I should be guaranteed to have a fi.col, fj.col, and constraints.
			
		if(graphs){
			fi.plot.info <- prettyPlot(res$fi,x_axis=x_axis,y_axis=y_axis,col=fi.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,pch=fi.pch,contributionCircles=TRUE,contributions=res$ci,dev.new=TRUE)
			if(!(class(res)[1]=='epMDS')){
				if(biplots){
					fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,axes=FALSE,contributionCircles=TRUE,contributions=res$cj,pch=fj.pch,dev.new=FALSE,new.plot=FALSE)
				}else{
					fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,pch=fj.pch,contributionCircles=TRUE,contributions=res$cj,dev.new=TRUE)		
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
					correlationPlotter(res$X,res$fi,col=fi.col,pch=fi.pch,x_axis=x_axis,y_axis=y_axis,xlab=xlab,ylab=ylab,main=main) 
				}else{
					correlationPlotter(res$X,res$fi,col=fj.col,pch=fj.pch,x_axis=x_axis,y_axis=y_axis,xlab=xlab,ylab=ylab,main=main) 
				}
			}
		}								
	}
	
	#this happens whether I graph, or not. 
	if(class(res)[1]=='epMDS'){
		epPlotInfo <- list(fi.col=fi.col,fi.pch=fi.pch,fj.col=fi.col,fj.pch=fi.pch,constraints=constraints)	
	}else{
		epPlotInfo <- list(fi.col=fi.col,fi.pch=fi.pch,fj.col=fj.col,fj.pch=fj.pch,constraints=constraints)
	}

	class(epPlotInfo) <- c("epGraphs", "list")
	return(epPlotInfo)	
}
