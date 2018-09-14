\name{kurtosis}

\alias{kurtosis}

\title{
  Calculate the Kurtosis of Set of Numeric Values
}

\description{
  Calculates the kurtosis of a set of \emph{numeric} values (assumed to be from a sample).
}

\usage{
kurtosis(x, na.rm=TRUE)
}

\arguments{
  \item{x}{
A \emph{numeric} vector.
  }
  \item{na.rm}{
  If \code{TRUE} (default) observations with a missing value of \code{x} are removed before applying the test.
  }
}


\value{
  \emph{Numeric}. Returns the value of the calculated kurtosis.
}

\note{
 A near direct copy of solution posted by Wolfgang Koller at \url{https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html}
}

\author{
Paul Williamson
}

\examples{
## Calculate the kurtosis of a sample
kurtosis(survey$Height)
}

\alias{kurtosis}
