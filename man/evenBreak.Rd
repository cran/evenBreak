\name{evenBreak}
\alias{evenBreak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
evenBreak
}
\description{
compute the probability of a given distribution of a single suit across 4 hands
}
\usage{
evenBreak(D4,n1,n2,n3,PERM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D4}{
one quarter of the size of the deck, normally = 13
}
  \item{n1}{
the number of cards in the suit in the hands of player 1
}
  \item{n2}{
the number of cards in the suit in the hands of player 2
}
  \item{n3}{
the number of cards in the suit in the hands of player 3
}
  \item{PERM}{
the number of permutations
}
}
\value{
returns the probability of a given distribution of a single suit across 4 hands
}
\author{
Barry Zeeberg
}
\examples{
n1<-3
n2<-3
n3<-3
n4<-13-(n1+n2+n3)
PERM<-length(unique(combinat::permn(c(n1,n2,n3,n4))))
PERM
evenBreak(13,n1,n2,n3,1)*length(unique(combinat::permn(c(n1,n2,n3,n4))))
}
