\name{tally}
\alias{tally}

\title{
  Create Count or Proportion Cross-tabulations with Optional Margins
}

\description{
  Creates an \emph{n}-way table (cross-tabulation) with options to display results as weighted or unweighted counts, proportions or percentages, to include column, row or joint table totals and to control the number of decimal places used when reporting the output.
}

\usage{
tally(formula=NULL, format="count", data=NULL, na.rm=FALSE,
      cumsum=FALSE, margin="none",
      dp=NULL, weights=NULL, flat=FALSE)
}

\arguments{
  \item{formula}{
A formula specifying the variable(s) to be included in the cross-tabulation. (See details.)
  }
  \item{format}{
  A character string specifying the type of output required. The default is \dQuote{\code{count}}. Other options are \dQuote{\code{proportion}} and \dQuote{\code{percentage}}.
  }
  \item{data}{
  A dataframe containing the variables to be cross-tabulated.
  }
  \item{na.rm}{
  A logical value (\code{TRUE}/\code{FALSE}) that specifies whether or not to remove missing values from the reported cross-tabulation. The default value is \code{FALSE}.
  }
  \item{cumsum}{
  A logical value (\code{TRUE}/\code{FALSE}) that specifies whether the cell values should be cumulated. The default value is \code{FALSE}. The way in which values are cumulated (across rows or down columns) is determined by \code{margin}.
  }
  \item{margin}{
  A character string specifying what type of table margins are required. The default value is \dQuote{\code{none}}. Other options are \dQuote{\code{row}}, \dQuote{\code{col}} and \dQuote{\code{joint}}.
}
  \item{dp}{
  Integer. The number of decimal places to which results will be reported. The default is \code{NULL}, which rounds percentages, proportions and weights counts to 2 d.p., but leaves unweighted counts unrounded.
  }
  \item{weights}{
  The name of a variable in the supplied dataframe to be used to weight the table. The default is \code{NULL} (no weight).
  }
  \item{flat}{
  A logical value indicating whether to return the table in \sQuote{flat} (\code{TRUE}) or \sQuote{list}(\code{FALSE}) format. The default value is \code{FALSE} (return table in list format).
  }
}

\details{
For univariate distributions, only include a variable name on the right-hand side of the the formula (e.g. \code{~ Tenure}). For bivariate distributions include a variable on both sides of the formula (e.g. \code{Health ~ Tenure}). The variable on the left defines the rows; the variable on the right defines the columns. For higher dimensional distributions, include additional variables on the right-hand side of the formula (e.g. \code{Health ~ Tenure + Sex}).
}

\value{
Returned as a \code{table} data object, unless \code{flat = TRUE}, in which case a \code{ftable} data object is returned.
}

\author{
Paul Williamson
}


\examples{

## n-way frequency counts
tally( ~Tenure, data=survey)
tally(Health ~ Tenure, data=survey)
tally(Health ~ Tenure + Sex, data=survey)

## Flat 3-way table
tally(Health ~ Tenure + Sex, data=survey, flat=TRUE)

## Add row/column/joint totals
tally(Health ~ Tenure, data=survey, margin = "row")
tally(Health ~ Tenure, data=survey, margin = "col")
tally(Health ~ Tenure, data=survey, margin = "joint")

## Report as percentages or proportions
tally(Health ~ Tenure, data=survey, margin = "col", format = "percent")
tally(Health ~ Tenure, data=survey, margin = "col", format = "proportion")

## Cumulative
tally(Health ~ Tenure, data=survey, margin = "col",
      format = "percent", cumsum=TRUE)

## Weighted
tally(Health ~ Tenure, data=survey, margin = "col",
      format = "percent", cumsum=TRUE, weight="Person_w0")

## Specify decimal places in reported output
tally(Health ~ Tenure, data=survey, margin = "col",
      format = "percent", dp=4)

}

\alias{tally}
