skew <-  function(x, na.rm=TRUE) {
  #Near direct copy of solution posted by Wolfgang Koller
  #https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html
  if (na.rm==TRUE) {
    x <- x[!is.na(x)]
  }
  m3 <- mean((x-mean(x))^3)
  skew <- m3/(sd(x)^3)
  return(skew)
}
