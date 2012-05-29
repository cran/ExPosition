pickSVD <-
function(datain,k=0){

	#if there are more than 2 million elements, do the eigen approach!
	if((dim(datain)[1] * dim(datain)[2]) > 2000000){
		dataDims <- dim(datain)
		I <- dataDims[1]
		J <- dataDims[2]

		m <- min(c(I,J))
		flip <- 0
		
		if (I < J){
			datain <- t(datain)
			flip <- 1
		}

		eigOut <- eigen(t(datain) %*% datain)
		Q <- eigOut$vectors
		D <- sqrt(eigOut$values)
		P <- datain %*% Q %*% solve(diag(D))

		if(flip==1){
			temp=Q
			Q=P
			P=temp
		}
		return(list(u=P,v=Q,d=D))
	}else{
		return(svd(datain))
	}
}
