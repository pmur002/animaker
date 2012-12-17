\name{frameTiming}
\alias{frameTiming}
\title{
  Retrieve animations for a specific time period
}
\description{
  When creating a frame-based animation we need to know what animations
  are due to be played at a given time. \code{frameTiming} subsets the
  timing information created by an animation to only those that need to
  be played at the current point in time.

  This is mostly just a convenience function for use with
  \code{\link{frameApply}}.
}
\usage{
frameTiming(x, time = 0)
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{time}{
    The time to match animations for.
  }
}
\value{
  A timing object.
}
\author{
  Paul Murrell.
}
