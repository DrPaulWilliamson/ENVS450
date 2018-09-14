\name{Tjur.Rsq}

\alias{Tjur.Rsq}

\title{
  Tjur's R-Square for Logistic Regression Models.
}

\description{
  Calculates Tjur's r-square for  a logistic regression model fitted to predict a binomial outcome.
}

\usage{
Tjur.Rsq(x)
}

\arguments{
  \item{x}{
The fitted \code{model} from a logistic regression.
  }
}


\value{
  \emph{Numeric}. Returns the value of the calculated Tjur's r-square.
}

\note{
  Tjur's r-square is the mean of the predicted outcomes for persons with a true outcome of 1, less the mean of the predicteed outcomes for persons with a true outcome of 0, multiplied by 100 to convert it into a percentage. 100\% = all outcomes correctly predicted.
}

\references{
 Allison P (2014) \sQuote{Measures of fit for logistic regression}, Paper 1485-2014, Statistical Horizons LLC and the University of Pennnsylvania.
 
 Tjur, T (2009) \sQuote{Coefficients of determination in logistic regression models—A new proposal: The coefficient of discrimination}, \emph{The American Statistician}, 63: 366-372.
}

\author{
Paul Williamson
}

\examples{
# Fit logistic regression model
model <- glm(Sex ~ Age, data=survey, family=binomial)

# Calculate Tjur's r-square for that model
Tjur.Rsq(model)
}

\alias{Tjur.Rsq}
