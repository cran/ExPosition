genPDQ <-
function(M=NULL,datain,W=NULL,decomp.approach='svd',k=0){
	return(basePDQ(datain,genFlag=TRUE,M=M,W=W,decomp.approach=decomp.approach,k=k))
}
