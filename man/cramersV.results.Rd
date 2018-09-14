\name{cramersV.results}

\alias{cramersV.results}

\title{
  Cramer's V Correlation Matrix Reformatted plus Statistical Significance
}

\description{
  Reports the statistical significance of the Cramer's V for each varaible pair in the correlation matrix returned by \code{cramersV}. Output is in 'long' format with one row per variable pair, allowing the the ordering of variable pairs by variable name or by size or p-value of Cramer's V.
}

\usage{
  cramersV.results(cormat, sort.by="r", data=NULL, var.name=NULL, na.rm=TRUE)
}


\arguments{
  \item{cormat}{
    The correlation matrix produced by the function \code{cramersV}
  }
  \item{sort.by}{
    Indicates the order in which the results are to be reported. See 'Details' for options.
  }
  \item{var.name}{
    If \code{NULL} (the default), then results are returned for all variable pairs. If a specific variable in \code{res} is named, then only results for pairs involving that variable are returned.
  }
  \item{data}{
   If \code{NULL} (the default), then \emph{p-values} and significance level of each correlation are not reported. If a data frame is supplied, results will include \emph{p-values} and signficance level for each correlation, provided the data frame includes all the cases use to produce the input correlation matri \code{res}.
   }
   \item{na.rm}{
    If \code{TRUE} (default) records with a missing value of \code{x} or \code{y} are removed before calculating statistical significance; if \code{FALSE}, then the value of Cramer's V is set to \code{NA} if there are one or missing \code{x} or \code{y} values. If \code{data = NULL} then the value of \code{na.rm} is inherited from the call to \code{cramers.V} that produced \code{res}.
  }
  
}

\details{
  \code{sort.by} offers three options. \dQuote{\code{r}} (the default) sorts results in decreasing order of Cramer's V; \dQuote{\code{x}} sorts results in alphabetic order of the \code{x} variable; \dQuote{\code{y}} sorts results in alphabetic order of the \code{y} variable.
}

\value{
  A dataframe reporting, for each variable pair:
  
  \item{r}{
    Cramer's V
  }
  
  \item{p.value}{
    \emph{p-value} [if \code{data != NULL}] 
  }
  
  \item{sig}{
    level of statistical significance (\code{sig}) [if \code{data != NULL}], where ** \emph{p} >= 0.99; * \emph{p} >= 0.95; else \emph{p} < 0.95 
  }

}

\note{
  Adapted from \url{http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need}
}

\author{
   Paul Williamson
}

\seealso{
  \code{\link{cramersV}}; \code{\link{cramersV.test}}
}

\examples{
# Default (using results from cramersV)
res <- cramersV(survey[, 3:8], fill=FALSE)
cramersV.results(res) # by default, results sorted by Cramer's V (r)

#  Sort results by variable name
cramersV.results( res, sort.by = "x" )
cramersV.results( res, sort.by = "y" )

# Reports only those results involving a specified variable
cramersV.results( res, var.name = "Tenure" )

# Report p-value and significance level
cramersV.results( res, data = survey[ , 3:8] )
cramersV.results( res, data = survey[ , 3:8], sort.by = "p.value" )

}

\alias{cramersV.results}
