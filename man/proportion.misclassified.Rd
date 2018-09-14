\name{proportion.misclassified}

\alias{proportion.misclassified}

\title{
  Calculate the Proportion Misclassifed
}

\description{
  Calculates the proportion misclassified for a set of \emph{categorical} values.
}

\usage{
proportion.misclassified(x, na.rm=TRUE)
}

\arguments{
  \item{x}{
A \emph{categorical} vector.
  }
  \item{na.rm}{
  If \code{TRUE} (default) observations with a missing value of \code{x} are removed before applying the test.
  }
}


\value{
  \emph{Numeric}. Returns the value of the calculated proportion misclassified. Multiply by 100 for the percentage misclassified.
}

\note{
 The proportion misclassified represents the percentage of observations that would have to swap category in order to achieve an even distribution of obsevations across categories. It is also known as the Index of Dissimilarity, althogh the IoD is normally used to compare the difference between the distribution of two groups across the same set of categories, rather than the difference of the distribution of one group and from a theoretical uniform distribution.
}

\author{
Paul Williamson
}

\examples{
## Calculate the proportion misclassifed
proportion.misclassified(survey$Age)
}

\alias{proportion.misclassified}
