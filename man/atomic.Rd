\name{atomic}
\alias{atomic}
\title{
  Create an atomic animation
}
\description{
  An atomic animation describes an animation by providing two key
  parameters, the starting time and the duration. A label can be given
  to describe what an animation is supposed to be doing.
}
\usage{
atomic(start = 0, durn = 0, label = labelSel())
}
\arguments{
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
  If \code{durn} is \code{NA}, the duration of the animation will fill
  to the remaining space in its container.
}
\value{
  An atomic animation object.
}
\author{
  Paul Murrell
}
