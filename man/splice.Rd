\name{splice}
\alias{splice}
\title{
  Splice new animations into an existing animation
}
\description{
  The purpose of this function is to be able to insert an animation into
  an existing container animation (i.e. a sequence or a track).
}
\usage{
splice(x, values, after = NULL, at = NULL)
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{values}{
    An animation object to splice into \code{x}.
  }
  \item{after}{
    The index after which \code{values} will be spliced into \code{x}.
    Causes a sequence animation to be created.
  }
  \item{at}{
    The index at which to add \code{values} into \code{x}. Causes a
    track animation to be created.
  }
}
\value{
  An animation object. Either a sequence animation (\code{vec}) or a
  track animation (\code{trac}).
}
\author{
  Paul Murrell.
}
