cor.results <- function(cormat, sort.by="r", data=NULL, var.name=NULL) {
  #Adapted from: http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
  
  ut <- upper.tri(cormat)
  result <- data.frame(
    x = rownames(cormat)[row(cormat)[ut]],
    y = rownames(cormat)[col(cormat)[ut]],
    r  = (cormat)[ut]
  )
  

  if (!is.null(data)) {
    
    for (i in 1:nrow(result)) {
      
      tmp <- cor.test( data[,as.character(result$x[i])], 
                       data[,as.character(result$y[i])] ) 
      
      result[i,"p.value"] <- round(tmp$p.value, 2)
      
      if (tmp$p.value <= 0.01) {
        result[i,"sig."] <- "**"        
      } else if(tmp$p.value <= 0.05) {
        result[i,"sig."] <- "*"
      } else {
        result[i,"sig."] <- ""
      }
      
      result[i,"95pct.ci.lb"] <- round( tmp$conf.int[[1]], 3 )
      result[i,"95pct.ci.ub"] <- round( tmp$conf.int[[2]], 3 )
      
    } #next correlation pair
    
  } #if a data.frame has been supplied
  
  if (!is.null(var.name)) {
    result <- result[ which( (result$x == var.name) | (result$y == var.name) ) , ]
    
    #Convert row and column into character variables to facilitate swapping contents between them
    result$x <- as.character(result$x)
    result$y <- as.character(result$y)
    
    #Go through each variable pair in turn, and ensure that var.name is listed under row,
    #with it's associated variable pair in 'column'
    for (i in 1:nrow(result)) {
      if (result$y[i] == var.name) {
        result$y[i] <- result$x[i]
        result$x[i] <- var.name
      }
    } #next variable pair
    
    #Convert row and column back to factors, using the same factor order for both
    result$x <- factor(result$x, levels=c(result$y,unique(result$x)))
    result$y <- factor(result$y, levels=c(result$y,unique(result$x)))
    
  } #if !is.null(var.name)
  
  
  # Sort results into order required by user [default = descending value of r]
  if (sort.by =="abs.r") {
    result <- result[ order( abs(result$r), decreasing = TRUE ), ]
  } else if (sort.by == "x") {
    result <- result[ order( result$x, decreasing = FALSE ), ]
  } else if (sort.by == "y") {
    result <- result[ order( result$y, decreasing = FALSE ), ]
  } else if (sort.by == "p.value") {
    result <- result[ order( result$p.value, decreasing = TRUE ), ]
  } else {
    result <- result[ order( result$r, decreasing = TRUE ), ]
  }
  
  
  #Align numbering of rows to rows left in data.frame
  rownames(result) <- seq(1:nrow(result))
  
  return(result)
  
}
