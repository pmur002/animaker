\name{dynPlot}
\alias{dynPlot}
\title{
  Draw an animated SVG plot of an animation
}
\description{
  This function creates an animated SVG file through the use of the
  \sQuote{gridSVG} package. Instead of simply drawing a plot showing a
  visual description, we also show how this animation occurs over time.
}
\usage{
dynPlot(x, file = "anim.svg", ...)
}
\arguments{
  \item{x}{
    An animation object.
  }
  \item{file}{
    A character vector specifying the file to save to.
  }
  \item{\dots}{
    Additional parameters passed onto specific plotting functions.
  }
}
\details{
  The use of \dots should be discouraged.
}
\value{
  None.
}
\author{
  Paul Murrell.
}
