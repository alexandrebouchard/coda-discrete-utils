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

#' Plot an approximate density from the output of jags.samples
#' ran a continuous model.
#' 
#' @param jags.output The output of jags.samples ran on a continuous model
coda.density <- function(jags.output)
{
  for (variable.name in names(jags.output)) 
  {
    coda.output <- as.mcmc.list(jags.output[[variable.name]])
    my.matrix <- as.matrix(coda.output)
    my.frame <- data.frame(x= my.matrix[,1])
    
    p <- ggplot(my.frame, aes(x)) + 
      geom_density(adjust=5, size=2) +
      labs(x = "x", y = "Approximation of f(x)") +
      ggtitle(paste("Density of ", variable.name, sep=""))
    ggsave(p, filename = paste(variable.name , "-density.pdf",sep=""))
  }
  
}


#' Output an estimate of the joint density of the two specified variables.
#' 
#' @param jags.output The output of jags.samples.
#' @param first.variable The first variable to plot
#' @param second.variable The second variable to plot
#' @param print.marginals Whether the marginals should be added to the top and right of the plot
coda.density2d <- function(jags.output, first.variable, second.variable, print.marginals = FALSE)
{ 
  coda.output1 <- as.mcmc.list(jags.output[[first.variable]])
  my.matrix1 <- as.matrix(coda.output1)
  
  coda.output2 <- as.mcmc.list(jags.output[[second.variable]])
  my.matrix2 <- as.matrix(coda.output2)
  
  my.frame <- data.frame(x= my.matrix1[,1],y= my.matrix2[,1])
  
  file.name <- paste(first.variable,"-",second.variable, "-joint-density.jpg",sep="")
  
  p <- ggplot(my.frame, aes(x,y)) + 
    stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
    labs(x = first.variable, y = second.variable) 
  
  if (print.marginals)
  {
    p <- p + theme(legend.position="none")
    
    #marginal density of x - plot on top
    plot_top <- ggplot(my.frame, aes(x)) +
      geom_density(adjust=5)
    
    #marginal density of y - plot on the right
    plot_right <- ggplot(my.frame, aes(y)) + 
      coord_flip() +
      geom_density(adjust=5)
    
    empty <- ggplot()+geom_point(aes(1,1), colour="white") +
      theme(                              
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
    
    #arrange the plots together, with appropriate height and width for each row and column
    jpeg(file.name)
    grid.arrange(plot_top, empty, p, plot_right, ncol=2, nrow=2, widths=c(2, 1), heights=c(1, 2))
    garbage <- dev.off();
  }
  else
  {  
    ggsave(p, filename = file.name)
  }
}


print.matrix <- function(m)
{
  write.table(m, row.names=F, col.names=T, quote=F)
}
