\name{multiplot}

\alias{multiplot}

\title{
  Combine Multiple Graphs into One Plot 
}

\description{
  Takes a set of plots and combines them into one \sQuote{multiplot}, allowing the user to specify how the graphs are laid out across the rows and columns of the plot. Specifically designed to overcome problems with more standard approaches to creating multiplots using \code{\link{ggplot2}} graphs.
}

\usage{
multiplot(..., plotlist=NULL, cols=1, layout=NULL)
}

\arguments{
  \item{...}{
A series of comma-separated plots. E.g. \code{plot1, plot2, plot3}
  }
  \item{plotlist}{
  A \code{list} data object comprising a series of plots. E.g. \code{list(plot1, plot2, plot3)}
  }
  \item{cols}{
  Numeric. The number of columns that the multiplot should use when arranging the plots. The default is \code{1}.
  }
  \item{layout}{
  A \code{Grid} layout object which splits the \code{viewport} into subregions, specifies the order in which the graphs should be plotted (by row; by column), and specifies which cells in the layout should be occupied by each graph. E.g. \code{matrix(1,2,3,3), nrow=2, byrow=TRUE}. See \code{\link{viewport}} for further details.
  }
}

\value{
Returns one plot.
}

\note{
Closely based upon \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
}

\author{
Paul Williamson
}

\seealso{
  \code{\link{grid}}; \code{\link{viewport}}
}

\examples{
## Create some ggplot graphs
load.package("ggplot2")
plot1 <- ggplot(data=survey) + geom_bar(aes(x=Health))
plot2 <- ggplot(data=survey) + geom_bar(aes(x=Tenure))
plot3 <- ggplot(data=survey) + geom_bar(aes(x=Age))

## Combine multiple ggplots in one plot
multiplot(plot1, plot2, plot3)

## Change number of columns used to create plot
multiplot(plot1, plot2, plot3, cols=2)

## Pass the plots as a list of plots
plots <- list(plot1, plot2, plot3)
multiplot(plotlist=plots, cols=3)

## Specify which cells in plot grid each plot occupies
multiplot(plotlist=plots, layout=matrix(c(1,2,3,3), nrow=2, byrow=TRUE))

}

\alias{multiplot}
