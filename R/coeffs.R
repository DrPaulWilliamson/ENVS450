#coeffs: a function to report glm() model coeffs + exp(B) + confint(exp(B))
coeffs <- function(model, conf.int=FALSE) {
  df <- data.frame(  summary(model)$coefficients, expB= exp(model$coefficients) )  
  names(df)[1:4] <- c("B","Std.Error","z.value","p.value")
  if (conf.int==TRUE) {
    df <- cbind(df, exp(confint.default(model)) )
    names(df)[6:7] <- c("CI.L95pct", "CI.U95pct")
  }
  return( df )
}