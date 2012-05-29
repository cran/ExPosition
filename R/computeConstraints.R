computeConstraints <- function(results,x_axis,y_axis){
			if(("fii") %in% names(results)){
				return(minmaxHelper(results$fii,results$fj,axis1=x_axis,axis2=y_axis))
			}else if(!("fj" %in% names(results))){
				return(minmaxHelper(results$fi,axis1=x_axis,axis2=y_axis))
			}else if("fi" %in% names(results)){
				return(minmaxHelper(results$fi,results$fj,axis1=x_axis,axis2=y_axis))
			}else{
				stop("Results are improperly formatted. computeConstraints must quit.")
			}	
}