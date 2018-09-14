#best.fit.line()
best.fit.line <- function(df=NULL, a, b) {

  #require(ggplot2)
    
  if (!is.null (df)) {
    var.name.x <- names(df)[1]
    var.name.y <- names(df)[2]
    names(df) <- c("x","y")
  } else {
    var.name.x <- "X"
    var.name.y <- "Y"
  }
  
  plot.points <- TRUE
  if ( is.null(df) )  {
    x <- seq(1:100)
    y <- seq(1:100)
    df <- data.frame(x=x, y=y)
    plot.points <- FALSE
  }
  
  df$best.fit.line <- a + (b*df$x)
  
  TSS <- sum((df$y - df$best.fit.line)^2)
  
  graph <- ggplot(data=df)
  
  if (plot.points)  {
    
    graph <- graph + geom_point( aes(x=x, y=y) ) + labs(x=var.name.x, y=var.name.y) +
      theme_bw()
    
  }
  
  graph <- graph + 
    geom_line( aes(x=x, y= best.fit.line), colour="blue")
  
  if (!plot.points)  {
    
    graph <- graph + labs(x="X", y="Y") + ylim(0,100) + xlim(0,100) + theme_bw()
    
  }
  
  print( graph )
  
  cat(paste(var.name.y, "= ", a, " + ", b, "(", var.name.x, ")"))
  cat("\n","\n")
  cat(paste("Total Sum of Squares = ",round(TSS,1)))
  
}