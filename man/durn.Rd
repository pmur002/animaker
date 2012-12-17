\name{durn}
\alias{durn}
\alias{durn<-}
\title{
  Getting and setting the duration of an animation
}
\description{
  Simple methods for accessing and manipulating the duration of an
  animation.
}
\usage{
durn(x)

durn(x) <- value
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{value}{
    The new animation duration.
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
durn(a) # 4
durn(a) <- 10
durn(a) # 10
}
