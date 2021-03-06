\name{timing}
\alias{timing}
\alias{timing.atomicAnim}
\alias{timing.vecAnim}
\alias{timing.tracAnim}
\title{
  Create a timing scheme from animation sequences
}
\description{
  The main purpose of creating animation sequences is to coordinate timing of
  animation actions. The timing method turns a description of an
  animation into a timing scheme.

  A timing scheme simply says when an animation occurs, for how long it
  should occur, and also the context in which the animation occurs. The
  context is only useful when \code{vec} and \code{trac} animations have
  been used. In the case of a sequence animation, it is able to tell us
  which iteration within a sequence an animation occurs in. For track
  animations it can tell us which track an animation occurs within.
}
\usage{
timing(x, ...)
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{\dots}{
    Parameters passed onto specific methods.
  }
}
\value{
  A timing object.
}
\author{
  Paul Murrell.
}
\examples{
a <- atomic(start = 2, durn = 4, label = "a")
b <- atomic(durn = 5, label = "b")
v <- vec(a, b)
timing(a)
timing(v)
}
