\name{group.data}

\alias{group.data}

\title{
  Group Continuous Data Using User-Friendly Class Labels
}

\description{
  Allocate continuous data into classes using user-friendly class labels.
}

\usage{
group.data(data=data, breaks=breaks, output.dp=NULL,
          include.lowest=TRUE, integer.dp=6, mid.point=FALSE)
}

\arguments{
  \item{data}{
  A vector of \emph{numeric} values
  }
  \item{breaks}{
  Either a single number specifiying the number of equal interval categories required, or a numeric vector specifying the required class boundaries, including the lower and upper boundaries. E.g. \code{c(0, 5, 10, 15)}. See details for further discussion.
  }
  \item{output.dp}{
  Number of decimal places to be used when reporting class boundaries. Default is retain precision of the input values (See Details)
  }
  \item{include.lowest}{
  logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included. Default is \code{TRUE}.
  }
  \item{integer.dp}{
  Threshold for identifying a set of \code{data} values as integer. The default is 6 (i.e. treat a set of values in which no value is >= 0.000001 as integer). See Details.
  }
  \item{mid.point}{
  Logical. If \code{TRUE} then class is labelled by mid-point rather than by class boundaries. Default is \code{FALSE}.
  }
}

\details{
  \code{group.data} is a wrapper for the \code{\link{cut}} function, designed to return more user-friendly class labels.
  
  If using \code{breaks} to supply a vector of class boundaries, it is important to ensure that these boundaries are specified to the same level of precision as the \code{data} values, otherwise values that fall between class boundaries will remain unclassified (\code{NA}). For example, if the data value is 1.972, then the class boundary 1.852-1.972 clearly includes the value, whereas the class boundaries of 1.85-1.97 and 1.98-2.3 leave the value unclassified.
  
  The \code{output.dp} option can be used to round \emph{reported} class boundaries to fewer decimal places than that used by the \code{data} values. This is designed to help greatly improve readability of outputs, but will create 'inter-interval' gaps. For example after rounding to 1 d.p. it would be unclear which of the two reported class boundaries 1.5-1.9 and 2.0-2.7 the data value 2.94 falls in. For this reason the default is for \code{group.data} to report class boundaries to the precision of the input data. Note that the value of \code{output.dp} has no effect on the precision of the class boundaries used internally to allocate values to classes. This is controlled via \code{breaks} as outlined above.
  
  For more information about \code{include.lowest} see the help for \code{\link{cut}}.
  
  \code{integer.dp} is provided in order to allow users to circumvent the common problem of (small) floating point errors in computer calculations leading to integer numbers being incorrectly classifed as real-valued numbers. \code{integer.dp} specifies a threshold for the fractional parts of a set of values below which they are to be treated as integer by truncating the fractional part of each value. For example, the default behaviour (\code{integer.dp = 6}) is to treat the set of values 1.0000009, 2.0000004 and 1.0000001 would be treated as the integer values 1, 2 and 1. 
  
}

\value{
Factor vector, providing one class label per \code{data} value. 
}


\author{
Paul Williamson
}


\seealso{
  \code{\link{cut}}
}

\examples{

## Group original data into five equal-interval categories
res <- group.data(data=survey$Height, breaks=5)

## compare original to grouped values
tail( data.frame(survey$Height, res) ) 

## Class boundaries specified to \emph{n} decimal places
res <- group.data(data=survey$Height, breaks=5, output.dp=4)

## Report class mid-point instead of upper and lower bounds
res <- group.data(data=survey$Height, breaks=5, mid.point=TRUE)

## Threshold for treating a data value as integer rather than continuous
res <- group.data(data=survey$Height, breaks=5, integer.dp=0)

## Exclude lowest x[i] equal to the lowest 'breaks' value
res <- group.data(data=survey$Height, breaks=5, include.lowest=FALSE, output=4)

}

\alias{group.data}
