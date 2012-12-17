\name{trac}
\alias{trac}
\title{
  Create a track animation
}
\description{
  Track animations are a collection of animations that play
  simultaneously. A track animation can have any type of animation as
  its children.
}
\usage{
trac(..., start = 0, durn = NULL, label = labelSel())
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
  A \code{trac} animation object.
}
\author{
  Paul Murrell.
}
