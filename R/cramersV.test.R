cramersV.test <- function(x, y, na.rm=TRUE) {
  
  if (na.rm==TRUE) {
    useNA <- "no"
  } else if (na.rm==FALSE) {
    useNA <- "ifany"
  }
  
  
  #If x or y is NOT a factor, convert into a factor if it has <= 20 unique values
  if ( !is.factor(x) ) {
    if ( length( unique(x) ) <= 20 ) x <- as.factor(x)
  }
  
  if ( !is.factor(y) ) {
    if ( length( unique(y) ) <= 20 ) y <- as.factor(y)
  }
  
  #If one of variable pair is still NOT a factor, return CV = NA;
  #else calculate Cramer's CV
  if (!is.factor(x) | !is.factor(y)) {
    cramer <- NA
    p.value <- NA
  } else {
    t <- table(x, y, useNA= useNA)
    chisq.results <- suppressWarnings(chisq.test(t))
    chisq <- chisq.results$statistic
    p.value <- chisq.results$p.value
    cramer <- sqrt(chisq / (NROW(x) * (min(dim(t)) - 1)))
  }
  df <- data.frame(cramers.v = cramer, p.value= p.value)
  row.names(df) <- 1
  return(df)
}
