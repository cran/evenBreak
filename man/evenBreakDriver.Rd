\name{evenBreakDriver}
\alias{evenBreakDriver}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
evenBreakDriver
}
\description{
loop through all possible distributions of a single suit across 4 hands
}
\usage{
evenBreakDriver(D4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D4}{
one quarter of the size of the deck, normally = 13
}
}
\value{
returns a table of the probability of a given distribution of a single suit across 4 hands, \cr
conditioned on the number of suits that are known to be evenly distributed. 
}
\author{
Barry Zeeberg
}
\examples{
evenBreakDriver(13)
}
