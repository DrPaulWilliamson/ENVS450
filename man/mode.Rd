\name{Mode}

\alias{Mode}

\title{
  Find the Mode of a Distribution
}

\description{
  Returns the modal value of a set of numerical or categorical values.
}

\usage{
mode(x, na.rm = FALSE)
}

\arguments{
  \item{x}{
A vector.
  }
  \item{na.rm}{
  If TRUE (default) observations with a missing value of \code{x} are removed before applying the test.
  }
}


\value{
  Returns the modal value (category)
}

\note{
 Modified version of \url{http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode} that is able to handled tied modal categories.
}

\author{
Paul Williamson
}

\examples{
## Find modal category of a categorical variable
mode(survey$Age)

## Tied modal values
mode(c(1, 2, 2, 3, 3, 4))

## Default handling of missing values
mode( c(1, 2, 3, 3, NA, NA, NA) )

## Mode excluding missing values
mode( c(1, 2, 3, 3, NA, NA, NA), na.rm=TRUE)
}

\alias{mode}
