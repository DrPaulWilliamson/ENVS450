\name{skew}

\alias{skew}

\title{
  Calculate the Skew of a Set of Numeric Values
}

\description{
  Calculates the skew of a set of \emph{numeric} values (assumed to be from a sample).
}

\usage{
skew(x, na.rm=TRUE)
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
  \emph{Numeric}. Returns the value of the calculated skew.
}

\note{
 A near direct copy of solution posted by Wolfgang Koller at \url{https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html}
}

\author{
Paul Williamson
}

\examples{
## Calculate the skew of a sample
skew(survey$Height)
}

\alias{skew}
