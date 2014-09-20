coda.pmf <- function(coda.output) {
  my.matrix <- as.matrix(coda.output)
  
  for (col.name in colnames(my.matrix)) 
  {
    print('plotting')
    my.freq <- freq(variable.data, plot=FALSE)
    number.rows <- dim(my.freq)[1]
    my.freq <- my.freq[-number.rows,]
    #pdf(paste(col.name,"-pmf.pdf",sep=""))
    x.values <- as.numeric(row.names(my.freq))
    y.values <- my.freq[,2]/100
    max.y <- max(y.values)
    plot(x.values,y.values,type="h",xlab="k",ylab=paste("Approximation of P(",col.name,"= k)",sep=""),main=NULL,ylim=c(0,max.y)) 
    title("Probability Mass Function (PMF)")
    garbage <- dev.off();
  }
}

