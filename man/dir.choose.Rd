\name{dir.choose}

\alias{dir.choose}

\title{
  Capture Full Pathname of Selected Directory/Folder
}

\description{
  Allows user to capture the full pathname of a selected directory/folder via a graphical user interface
}

\usage{
dir.choose()
}

\arguments{
  None
}

\details{
  Requires the package \code{tckltk}, which is installed and loaded into memory by \code{dir.choose} as necessary
}

\value{
Returns full pathname of chosen directory/folder as a character string
}


\seealso{
  \code{\link{file.choose}}
}

\author{
Paul Williamson
}


\examples{

## Not run:
res <- dir.choose()

}

\alias{dir.choose}
