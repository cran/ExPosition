mdsTransform <- function(D,MW){
	DATA_dimensions <- dim(D)
	#do this every time.
	Mrepmat <- matrix(MW$M,nrow=nrow(D),ncol=ncol(D))
	if(is.null(dim(MW$M))){ # it is a vector; with new way, it is always a vector
		BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% MW$M)
	}else{#ths forces a matrix to be a vector this needs to be better.
		BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% diag(MW$M))
	}
	S <- -.5 * sqrt(Mrepmat) * BigXi %*% D %*% t(BigXi) * sqrt(t(Mrepmat))
	rownames(S) <- rownames(D)
	colnames(S) <- colnames(D)
	return(S)
}