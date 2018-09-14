\name{load.package}
\alias{load.package}

\title{
  Download Package and Load into Memory
}

\description{
  Loads requested package into memory via the \code{library} function, first using \code{install.packages} to download requested pacakge if not already downloaded.
}

\usage{
load.package(x, mirror=getOption("repos")[[1]])
}

\arguments{
  \item{x}{
The name of the package to be loaded, as a character string.
  }
  \item{mirror}{
  The name of the CRAN mirror site from which any package that needs installing is to be sourced. The default is to use the currenly assigned mirror sits using the command \code{getOption("repos")[[1]]}. See Details. 
  }
}

\details{
  If the \code{mirror} is undefined or unresponsive, an alternative mirror can be specified. E.g. \code{mirror = "http://www.stats.bris.ac.uk/R/"}. For a full set of available mirror sites, see \url{https://cran.r-project.org/mirrors.html}.
}

\value{
None
}


\author{
Paul Williamson
}


\examples{

## Not run:
\dontrun{load.package("ENVS450")}

}

\alias{load.package}
