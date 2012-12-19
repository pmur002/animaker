\name{plot.anim}
\alias{plot.anim}
\title{
  Draw a diagram of an animation
}
\description{
  This function produces a graphical diagram to show the
  start times and durations of all atomic animations in an
  animation sequence.
}
\usage{
\method{plot}{anim}(x, ...)
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{\dots}{
    Not really used at the top level.
  }
}
\details{
  The use of \dots should be discouraged.
}
\value{
  None.
}
\seealso{
  \code{\link{dynPlot}} for producing animated diagrams.
}
\author{
  Paul Murrell.
}
\examples{\dontrun{
a <- atomic(start = 2, durn = 4, label = "a")
b <- atomic(durn = 3, label = "b")
v <- vec(a, b)
plot(v)
}}
