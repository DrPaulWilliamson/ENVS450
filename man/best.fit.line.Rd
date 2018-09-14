\name{best.fit.line}
\alias{best.fit.line}

\title{
  Create Scatterplot with User-Defined Line of Best-Fit
}

\description{
  Scatterplot with intercept and slope of \sQuote{best fit} line specified by user.
}

\usage{
best.fit.line(df=NULL, a, b)
}

\arguments{
  \item{df}{
A dataframe comprising 2 numeric vectors, with values of X in the first column and values of Y in the second.
  }
  \item{a}{
  The intercept for the line of fit to be added to a scatterplot of Y given X
  }
  \item{b}{
  The slope for the line of fit to be added to a scatterlot of Y given X
  }
}

\value{
Returns a scatterplot, with added line of fit with intercept \emph{a} and slope \emph{b}. Also reports the equation of line being fitted, plus total sum of squares for that line.
}


\author{
Paul Williamson
}


\examples{
best.fit.line(survey[ , c("Height", "Income")], 28000, -20)
}

\alias{best.fit.line}
