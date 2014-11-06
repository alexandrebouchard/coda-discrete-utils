#' Plot an approximate PMF (probability mass function) from the output of jags.samples
#' ran a discrete model.
#' 
#' @param jags.output The output of jags.samples ran on a discrete model
#' @param show.table Whether a table should be printed to standard out as well.
coda.pmf <- function(jags.output, show.table = FALSE) 
{ 
  for (variable.name in names(jags.output)) 
  {
    coda.output <- as.mcmc.list(jags.output[[variable.name]])
    my.matrix <- as.matrix(coda.output)
    variable.data <- my.matrix[,1]
    my.freq <- freq(variable.data, plot=FALSE)
    number.rows <- dim(my.freq)[1]
    my.freq <- my.freq[-number.rows,]
    pdf(paste(variable.name,"-pmf.pdf",sep=""))
    x.values <- as.numeric(row.names(my.freq))
    y.values <- my.freq[,2]/100
    max.y <- max(y.values)
    y.label <- paste("Approximation of P(",variable.name," = k)",sep="")
    plot(x.values,y.values,type="h",xlab="k",ylab=y.label,main=NULL,ylim=c(0,max.y)) 
    title("Probability Mass Function (PMF)")
    garbage <- dev.off();
    
    if (show.table)
    {
      m <- cbind(x.values, y.values)
      colnames(m) <- c("k", y.label)
      print.matrix(m)
      cat("\n")
    }
  }
}

#' Plot an approximate CDF (cumulative distribution function) from the output of jags.samples.
#' 
#' @param jags.output The output of jags.samples.
#' @param show.table Whether a table should be printed to standard out as well.
coda.cdf <- function(jags.output, show.table = FALSE) 
{  
  for (variable.name in names(jags.output)) 
  {
    coda.output <- as.mcmc.list(jags.output[[variable.name]])
    my.matrix <- as.matrix(coda.output)
    variable.data <- my.matrix[,1]
    mycdf <- ecdf(variable.data)
    pdf(paste(variable.name,"-cdf.pdf",sep=""))
    colnames(mycdf) <- NULL 
    rownames(mycdf) <- NULL
    y.label <- paste("Approximation of P(",variable.name," <= k)",sep="")
    plot(mycdf,xlab="k", ylab = y.label, main = NULL)
    title("Cumulative Distribution Function (CDF)")
    garbage <- dev.off();
    
    if (show.table)
    {
      my.freq <- freq(variable.data, plot=FALSE)
      number.rows <- dim(my.freq)[1]
      my.freq <- my.freq[-number.rows,]
      x.values <- as.numeric(row.names(my.freq))
      m <- cbind(x.values, mycdf(x.values))
      colnames(m) <- c("k", y.label)
      print.matrix(m)
      cat("\n")
    }
  }
}

#' Output the expectation of all the variables from the output of jags.samples.
#' 
#' @param jags.output The output of jags.samples.
coda.expectation <- function(jags.output) 
{  
  result <- c()
  for (variable.name in names(jags.output)) 
  {
    coda.output <- as.mcmc.list(jags.output[[variable.name]])
    my.matrix <- as.matrix(coda.output)
    variable.data <- my.matrix[,1]
    result[[variable.name]] <- mean(variable.data)
  }
  return(result)
}

#' Output the variance of all the variables from the output of jags.samples.
#' 
#' @param jags.output The output of jags.samples.
coda.variance <- function(jags.output) 
{  
  result <- c()
  for (variable.name in names(jags.output)) 
  {
    coda.output <- as.mcmc.list(jags.output[[variable.name]])
    my.matrix <- as.matrix(coda.output)
    variable.data <- my.matrix[,1]
    result[[variable.name]] <- var(variable.data)
  }
  return(result)
}

#' Output an estimate of the joint density of the two specified variables.
#' 
#' @param jags.output The output of jags.samples.
#' @param first.variable The first variable to plot
#' @param second.variable The second variable to plot
coda.density2d <- function(jags.output, first.variable, second.variable)
{ 
  coda.output1 <- as.mcmc.list(jags.output[[first.variable]])
  my.matrix1 <- as.matrix(coda.output1)
  
  coda.output2 <- as.mcmc.list(jags.output[[second.variable]])
  my.matrix2 <- as.matrix(coda.output2)
  
  my.frame <- data.frame(x= my.matrix1[,1],y= my.matrix2[,1])
  
  p <- ggplot(my.frame, aes(x,y)) + 
    stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
    labs(x = first.variable, y = second.variable)
  ggsave(filename = paste(first.variable,"-",second.variable, "-joint-density.pdf",sep=""))
}


print.matrix <- function(m)
{
  write.table(m, row.names=F, col.names=T, quote=F)
}
