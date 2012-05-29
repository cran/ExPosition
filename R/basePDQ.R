basePDQ <-
function(datain,k=0,genFlag=FALSE,vectorflag=FALSE,M=NULL,W=NULL){

	#check if M & W are OK
	if(is.null(M) || is.null(W)){
		genFlag <- FALSE
		vectorflag <- FALSE
	}
	
	#if GSVD, correct data.
	if(genFlag){
		if((is.null(dim(M)) && is.null(dim(W))) && (length(W)>0 && length(M)>0)){
			vectorflag <- TRUE
			datain <- matrix(M^(1/2),length(M),dim(datain)[2],byrow=FALSE) * datain * matrix(W^(1/2),dim(datain)[1],length(W),byrow=TRUE)
		}else if(length(dim(M))==2 && length(dim(W))==2){
			datain <- (M^(1/2)) %*% datain %*% (W^(1/2))		
		}else{
			stop("There is an error in the formatting of your masses or weights")
		}
	}

	#shipping off the call!	
	svdOUT <- pickSVD(datain)
	
	#now get data into PDQ
	P <- svdOUT$u
	d <- as.vector(svdOUT$d)	
	Q <- svdOUT$v		
	
	#check k
	if(k < 1){
		k <- length(d)
	}
	
	#find precision limit, fix what comes back.
	precisionLimit <- 2*.Machine$double.eps		
	indToKeep <- which(d^2 > precisionLimit)
	indToKeep <- indToKeep[1:min(c(length(indToKeep),k))]
	
	#if GSVD, correct data.
	if(genFlag){
		if(vectorflag){
			P <- matrix(M^(-1/2),dim(P)[1],dim(P)[2],byrow=FALSE) * P
			Q <- matrix(W^(-1/2),dim(Q)[1],dim(Q)[2],byrow=FALSE) * Q			
		}else{
			P <- diag((diag(M)^(-1/2))) %*% P
			Q <- diag((diag(W)^(-1/2))) %*% Q
		}
	}
	
	#send it all home!
	d <- d[indToKeep]	
	D <- diag(d)	
	P <- P[,indToKeep]
	Q <- Q[,indToKeep]
	
	res <- list(p=P,q=Q,Dv=d,Dd=D,ng=length(d))
	class(res) <- c("epSVD","list")	
	return(res)
}
