# load.package() - function to auto-install and load named packages as required

load.package <- function(x, mirror=getOption("repos")[[1]]) {
  #This function combines the tasks of:
  # - checking whether a package() has already been downloaded onto the PC
  # - downloading the package if not
  # - loading the package into memory [using require() rather than library()]
  
  #Code courtesy of Sacha Epskamp 
  # <http://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages>
  
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies=TRUE, repos=mirror)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
  
}