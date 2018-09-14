dir.choose <- function() {
  #A platform independent function to capture the full pathname of a directory;
  #designed to complement the existing file.choose() function
  
  #Inspired by desertnaut's response to stackoverflow:
  #http://stackoverflow.com/questions/33511964/r-windows-os-choose-dir-file-chooser-wont-open-at-working-directory
  #Plus Sacha Epskamp's code for function 'load.packages' to auto-install tckltk, as required: 
  #<http://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages>
  
  #1. Install and load the package 'tcltk' as necessary
  #if (!require("tcltk",character.only = TRUE))
  #{
  #  install.packages("tcltk",dep=TRUE,repos='http://www.stats.bris.ac.uk/R/')
  #  if(!require(tkcltk,character.only = TRUE)) stop("Package not found")
  #}
  
  #2. Prompt user to identify required directory
  folder <- tclvalue(tkchooseDirectory())
  
}
