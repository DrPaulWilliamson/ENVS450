\name{cor.results}

\alias{cor.results}

\title{
  Reformatted Correlation Matrix with \emph{p}-Values and Confidence Intervals
}

\description{
  Reformat correlation matrix from \code{cor} into variable-pair format, with options to calulate statistical significance and confidence intervals and sort results by required result attribute.
}

\usage{
cor.results(cormat, sort.by="r", data=NULL, var.name=NULL)
}

\arguments{
  \item{cormat}{
  A matrix of correlation results returned from the \code{cor} function
  }
  \item{sort.by}{
  Specify what order variable-pairs are reported in. Default value of \dQuote{\code{r}} sorts results in descending order of correlation coefficient. Other options are to sort by absolute value of \code{r} (\dQuote{\code{abs.r}}), sort by \emph{p-value} (\dQuote{\code{p.value}}) or sort by the names of variables in the \dQuote{\code{x}} or \dQuote{\code{y}} columns.
  }
  \item{data}{
  Default = \code{NULL}. If a data frame is supplied, then \emph{p-values} and confidence intervals are calculated for each variable-pair. Note that data frame specified must be the same one used by \code{cor} to produce the correlation matrix being reformatted.
  }
  \item{var.name}{
  Default = \code{NULL}. If a variable name is supplied, results are reported only for variable pairs including that variable.
  }
}

\value{
Returns a data frame identifying the names of each variable pair, the correlation between them and, optionally, the associted \emph{p-value}, significance level and confidence intervals.
}

\note{
Borrows heavily from http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
}

\author{
Paul Williamson
}


\seealso{
  \code{\link{cor}}; \code{\link{cor.test}}
}
\examples{

## Generate correlation matrix using the function cor( )
res <- cor(survey[, 8:11]) # use continuous variables only

## convert correlation matrix into 'variable-pair' format
cor.results(res)

## Sort results as required
cor.results(res, sort.by = "abs.r") # by absolute value of r
cor.results(res, sort.by = "x") # by names of 'x' variables
cor.results(res, sort.by = "y") # by names of 'y' variables

## Calculate and report p-value, significance level and confidence intervals
cor.results(res, data = survey[, 8:11])

## Report results for variable pairs involving a specific variable
cor.results(res, var.name="Height")

## Combine elements of function as required
cor.results(res, data = survey[ , 8:11], var.name="Height", sort.by = "p.value")

}

\alias{cor.results}
