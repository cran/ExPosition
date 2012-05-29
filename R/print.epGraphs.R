print.epGraphs <-
function (x,...) {

#list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=taus,M=M,W=W,pdq=pdqFIN)

  res.epGraphs <- x
  if (!inherits(res.epGraphs, "epGraphs")) stop ("no convenient data")
  cat("**ExPosition plotting data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(3, 2), list(1:3, c("name", "description")))
  
  res[1,] <- c("$fi.col","The colors for the row items.")
  res[2,] <- c("$fj.col","The colors for the column items.")
  res[3,] <- c("$constraints","Plotting constraints for axes.")  
  
  print(res)

}
