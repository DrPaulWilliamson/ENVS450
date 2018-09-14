mode <- function(x, na.rm = FALSE) {
  #Function to find and return the modal cateogry(ies)
  #with an option to include/exclude NAs
  #Adding the capability of handling tied modal categories to Gregor's suggested function at
  #http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  
  if(na.rm){
    x <- x[!is.na(x)]
  }
  
  ux <- unique(x)
  tx <- tabulate(match(x, ux))
  return(ux[which(tx==max(tx))])
}