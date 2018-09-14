\name{cramersV}
\alias{cramersV}

\title{
  Calculate Cramer's V
}

\description{
  Returns the value of Cramer's V for each variable pair in the supplied data frame
}

\usage{
cramersV(y, fill = TRUE, na.rm = TRUE)
}

\arguments{
  \item{y}{
A data frame comprising 2+ variables
  }
  \item{fill}{
  Default value \code{TRUE}. If \code{FALSE}, then only correlations in the upper-right hand of the correlation matrix are returned with all other values in the matrix being set to \code{NA}.
  }
  \item{na.rm}{
  If TRUE (default) observations with a missing value of \code{x} or \code{y} are removed before applying the test; else if \code{FALSE} returns a value of \code{NA} for each variable pair with one or more missing \code{x} or \code{y} values.
  }
}

\details{
  For each variable pair the value of Cramer's V is calculated via a call to the function \code{cramersV.test}. 
  
  For a variable pair including a continuous variable Cramer's V is undefined (\code{NA}). Note, however, that continuous variables with up to 20 unique values are treated as categorical, allowing a valid value of Cramer's V to be returned.
}

\value{
\code{cramersV}   Returns a data frame storing the calculated Cramer's V correlation matrix.
}

\author{
Paul Williamson
}


\seealso{
  \code{\link{cramersV.test}}; \code{\link{cramersV.results}}
}
\examples{

## Default setting
cramersV(survey[, 3:8])

## fill = FALSE
cramersV(survey[, 3:8], fill=FALSE)

}

\alias{cramersV}
