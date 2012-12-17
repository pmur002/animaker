\name{start}
\alias{start}
\alias{start<-}
\title{
  Getting and setting the starting time of an animation
}
\description{
  Simple methods for accessing and manipulating the starting time of an
  animation.
}
\usage{
start(x)

start(x) <- value
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{value}{
    The new animation starting time.
  }
}
\value{
  None.
}
\author{
  Paul Murrell.
}
\examples{
a <- atomic(start = 2, durn = 4, label = "a")
b <- atomic(durn = 5, label = "b")
v <- vec(a, b)
start(v) # 0
start(v) <- 10
start(v) # 10
}
