genPDQ <-
function(M=NULL,datain,W=NULL,k=0){
	return(basePDQ(datain,k=k,genFlag=TRUE,M=M,W=W))
}
