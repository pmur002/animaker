\name{export}
\alias{export}
\alias{export.anim}
\alias{export.timing}
\title{
  Export timing schemes to JSON
}
\description{
  There are forms of animation that are simply not possible to do with R
  graphics. In order to use the same timing information in another
  graphics system we export timing schemes to JSON, a simple and
  convenient data exchange format.
}
\usage{
export(x, jsVar = "timingData", file = "timingData.js")

\method{export}{anim}(x, jsVar = "timingData", file = "timingData.js")

\method{export}{timing}(x, jsVar = "timingData", file = "timingData.js")
}
\arguments{
  \item{x}{
    An animation object or a timing object.
  }
  \item{jsVar}{
    A character vector. The JavaScript variable that the resulting JSON
    is assigned to. If \code{NULL} or \code{""}, do not assign to a
    variable.
  }
  \item{file}{
    A character vector naming the file to save the JSON to. If this is
    \code{NULL} or \code{""} the JSON is returned as a character vector
    instead of being saved to a file.
  }
}
\value{
  By default, none. If \code{file} is \code{NULL} or \code{""}, a
  character vector.
}
\author{
  Simon Potter.
}
\examples{
a <- atomic(start = 2, durn = 4, label = "a")
b <- atomic(durn = 3, label = "b")
v <- vec(a, b)
export(timing(v), jsVar = "vTiming")
export(v, jsVar = "vTiming", file = "animTiming.js")
}
