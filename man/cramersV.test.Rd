\name{cramersV.test}

\alias{cramersV.test}

\title{
  Test Cramer's V for Statistical Significance
}

\description{
  Calculates the Cramer's V measure of correlation between \code{x} and \code{y} and the statistical significance of this correlation.
}

\usage{
  cramersV.test(x, y, na.rm=TRUE)
}

\arguments{
  
  \item{x}{
    A vector of factor, character, logical or numeric values.
  }
  
  \item{y}{
    A vector of factor, character, logical or numeric values.
  }
  \item{na.rm}{
    If \code{TRUE} (default) records with a missing value of \code{x} or \code{y} are removed before applying the test; if \code{FALSE}, then the value of Cramer's V is set to \code{NA} if there are one or missing \code{x} or \code{y} values.
  }
}

\details{
  If either \code{x} or \code{y} is supplied as a non-factor variable, and has <= 20 unique values, then it is converted into a factor to allow the value of Cramer's V and its significance to be calculated. If \code{x} or \code{y} is a non-factor variable with > 20 unique values then Cramer's V is not calculated and a value of \code{NA} is returned.
}

\value{
  A data frame returning two values:
    
  \item{cramers.v}{
  The Cramer's V measure of correlation between \code{x} and \code{y}
  }
    
  \item{p.value}{
    The statistical significance of the observed correlation
  }
  
}


\author{
   Paul Williamson
}

\seealso{
  \code{\link{cramersV}}; \code{\link{cramersV.results}}
}

\examples{

## Compare two factor variables
cramersV.test(survey$Tenure, survey$Age)

## Include a non-factor variable (income) with <= 20 unique values
cramersV.test(survey$Tenure[1:20], survey$Income[1:20])

## Include a non-factor variable (income) with > 20 unique values
cramersV.test(survey$Tenure, survey$Income)

}

\alias{cramersV.test}
