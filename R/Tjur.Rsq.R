#Tjur's Coefficient of Determination (after Allison, 2014)

Tjur.Rsq <- function(x) {
  df <- data.frame(y= x$model[,1], p=x$fitted.values)
  tmp <- aggregate(p ~ y, data=df, FUN=mean)
  return( abs(tmp$p[1] - tmp$p[2]) * 100 )
}
