#' Plot an approximate PMF (probability mass function) from the output of jags.samples
#' ran a discrete model.
#' 
#' @param jags.output The output of jags.samples ran on a discrete model
coda.pmf <- function(jags.output) 
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
    plot(x.values,y.values,type="h",xlab="k",ylab=paste("Approximation of P(",variable.name,"= k)",sep=""),main=NULL,ylim=c(0,max.y)) 
    title("Probability Mass Function (PMF)")
    garbage <- dev.off();
  }
}

#' Plot an approximate CDF (cumulative distribution function) from the output of jags.samples
#' ran a discrete model.
#' 
#' @param jags.output The output of jags.samples ran a discrete model.
coda.cdf <- function(jags.output) 
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
    plot(mycdf,xlab="k",ylab=paste("Approximation of P(",variable.name,"<= k)",sep=""),main=NULL)
    title("Cumulative Distribution Function (CDF)")
    garbage <- dev.off();
  }
}

#' Output the expectation of all the variables from the output of jags.samples
#' (not necessarily on a discrete model)
#' 
#' @param jags.output The output of jags.samples
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
  result
}

#' Output the variance of all the variables from the output of jags.samples
#'(not necessarily on a discrete model)
#' 
#' @param jags.output The output of jags.samples
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
  result
}

