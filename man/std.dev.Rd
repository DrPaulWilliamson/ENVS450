\name{std.dev}

\alias{std.dev}

\title{
  Calculate a Sample or Population Standard Deviation
}

\description{
  Calculates the standard deviation of a set of \emph{numeric} values, making an appropriate adjustment if using population rather than sample data.
}

\usage{
std.dev(x, na.rm=TRUE, sample=TRUE)
}

\arguments{
  \item{x}{
A \emph{numeric} vector.
  }
  \item{na.rm}{
  If \code{TRUE} (default) observations with a missing value of \code{x} are removed before applying the test.
  }
  \item{sample}{
  If \code{TRUE} (default), calculation uses \emph{n}-1 in the denominator. If \code{FALSE}, uses \emph{n} instead, as per standard statistical theory.
  }
}


\value{
  \emph{Numeric}. Returns the value of the calculated standard deviation.
}

\note{
 The \code{\link{sd}} function supplied as part of the base R x\code{stats} package assumes that the data are from a sample and offers no option to calculate the correct standard deviation for population data. The \code{std.dev} function offers this additional facility.
}

\author{
Paul Williamson
}

\examples{
## Calculate standard deviation of a sample (default)
std.dev(survey$Height)

## For a sample, std.dev returns same result as stats:::sd function
std.dev(survey$Height) == sd(survey$Height)

## Calculation standard deviation of a population
std.dev(survey$Height, sample = FALSE)

}
\alias{st.dev}
