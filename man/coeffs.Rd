\name{coeffs}

\alias{coeffs}

\title{
  GLM Coefficients Summary
}

\description{
  Reports GLM regression model coefficients alongside associated standard error, z-score, p-value and odds-ratio [exp(B)], plus, optionally, confidence intervals.
}

\usage{
coeffs(model, conf.int=FALSE)
}

\arguments{
  \item{model}{
The fitted \code{model} from a general linear model.
  }
  \item{conf.int}{
  If \code{TRUE} the confidence intervals of each regression coefficient are reported in addition to the standard outputs. The default (\code{FALSE}) is not to report the confidence intervals.
  }
}


\value{
  A data frame (one row per regression coefficient) comprising:
  
  \item{B}{
    Regression coefficient
  }
  \item{Std.Error}{
    The standard error of the regression coefficient
  }
  \item{z.value}{
    The z-score of the standard error
  }
  \item{p.value}{
    The significance level (\emph{p}-value) of the regression coefficient
  }
  \item{expB}{
    The odds ratio (exponent of the regression coefficient)
  }
  \item{CI.L95pct}{
    The lower bound of the 95\% Confidence Interval
  }
  \item{CI.U95pct}{
    The upper bound of the 95\% Confidence Interval
  }
}

\author{
Paul Williamson
}

\examples{
## Fit logistic regression model
model <- glm(Sex ~ Age, data=survey, family=binomial)

## Standard function output
coeffs(model)

## Standard output plus confidence intervals
coeffs(model, conf.int=TRUE)
}

\alias{coeffs}
