\name{compareProbs}
\alias{compareProbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compareProbs
}
\description{
organizes invoking evenBreakDriver() for 4 deck sizes, and presenting the results in a table \cr
and in a graph scatter plot of 2 sets of distribution probabilities
}
\usage{
compareProbs(verbose)
}
\arguments{
  \item{verbose}{
Boolean if TRUE print output data
}
}
\value{
returns no value, but has side effects of printing a table and a graph \cr
of the probability of a given distribution of a single suit across 4 hands, \cr
conditioned on the number of suits that are known to be evenly distributed. 
}
\author{
Barry Zeeberg
}
\examples{
compareProbs()
}
