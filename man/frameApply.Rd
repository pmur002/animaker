\name{frameApply}
\alias{frameApply}
\title{
  Create a frame-based animation
}
\description{
  A frame-based animation is one where we need to draw the entire state
  of the animation at regular intervals in time.  By repeatedly drawing
  at a fast enough rate, we can create the illusion of smooth animation
  if desired.

  This function works well in conjunction with the \sQuote{animation}
  package.
}
\usage{
frameApply(x, FUN = print, fps = 1, pause = TRUE)
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{FUN}{
    The function to call
  }
  \item{fps}{
    The number of frames to draw per second.
  }
  \item{pause}{
    Logical. Determines whether to sleep in-between drawing frames.
  }
}
\value{
  None.
}
\author{
  Paul Murrell.
}
