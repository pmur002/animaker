\name{vec}
\alias{vec}
\title{
  Create a sequence animation
}
\description{
  Sequence animations are a collection of animations where each
  animation occurs one after the other. A sequence animation can have
  any type of animation as its children.
}
\usage{
vec(..., start = 0, durn = NULL, label = labelSel())
}
\arguments{
  \item{\dots}{
    Animation objects to add as children.
  }
  \item{start}{
    The starting time of the animation.
  }
  \item{durn}{
    The duration of the animation.
  }
  \item{label}{
    A label used to describe the animation.
  }
}
\details{
  When the duration is \code{NULL}, the duration of the animation is
  inferred from its children.

  If \code{start} or \code{durn} is \code{NULL}, they then refer to the
  starting times and durations of their children. The starting times are
  relative to the starting times of the track animation itself.
}
\value{
  A \code{vec} animation object.
}
\author{
  Paul Murrell.
}
\examples{
a <- atomic(start = 2, durn = 4, label = "a")
b <- atomic(durn = 3, label = "b")
v <- vec(a, b)
v
}
