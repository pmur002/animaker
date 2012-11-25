
# Convenience functions for generating containers

# A sequence is a container made by putting anims in a sequence
vec <- function(..., start=0, durn=NULL, label=labelSel()) {
    anims <- list(...)
    x <- container(anims, start, durn, label)
    class(x) <- c("vecAnim", class(x))
    x
}

# A track is a container made by putting anims in parallel
trac <- function(..., start=0, durn=NULL, label=labelSel()) {
    anims <- list(...)
    x <- container(anims, start, durn, label)
    class(x) <- c("tracAnim", class(x))
    x
}

# start() gives the start of the *container*
# starts() gives the starts of the container *contents*
start.vecAnim <- function(x) {
    if (length(x$start) > 1) {
        0
    } else {
        x$start
    }
}

start.tracAnim <- function(x) {
    if (length(x$start) > 1) {
        0
    } else {
        x$start
    }
}

# durn() gives the duration of the *container*
# durns() gives the durations of the container *contents*
durn.vecAnim <- function(x) {
    if (is.null(x$durn) || length(x$durn) > 1) {
        durns <- durns(x)
        starts <- starts(x)
        n <- length(x$anim)
        starts[n] + durns[n]
    } else {
        x$durn
    }
}

durn.tracAnim <- function(x) {
    if (is.null(x$durn) || length(x$durn) > 1) {
        durns <- durns(x)
        starts <- starts(x)
        max(starts + durns)
    } else {
        x$durn
    }
}

"start<-" <- function(x, value) {
    UseMethod("start<-")
}

"start<-.containerAnim" <- function(x, value) {
    x$start <- value
    validateContainer(x)
    x
}

"durn<-" <- function(x, value) {
    UseMethod("durn<-")
}

"durn<-.containerAnim" <- function(x, value) {
    x$durn <- value
    validateContainer(x)
    x
}

# durn() gives the duration of the *container*
# durns() gives the durations of the container *contents*
durns <- function(x) {
    UseMethod("durns")
}

durns.containerAnim <- function(x) {
    if (is.null(x$durn) ||
        length(x$durn) == 1) {
        sapply(x$anims, durn)
    } else {
        x$durn
    }
}

# start() gives the start of the *container*
# starts() gives the starts of the container *contents*
starts <- function(x) {
    UseMethod("starts")
}

starts.vecAnim <- function(x) {
    if (length(x$start) == 1) {
        starts <- cumsum(sapply(x$anims, start))
    } else {
        starts <- cumsum(x$start)
    }
    n <- length(x$anims)
    if (n > 1) {
        durns <- durns(x)
        starts + cumsum(c(0, durns[1:(n - 1)]))
    } else {
        starts
    }
}

starts.tracAnim <- function(x) {
    if (length(x$start) == 1) {
        sapply(x$anims, start)
    } else {
        x$start
    }
}

as.character.vecAnim <- function(x, ...) {
    animChar <- lapply(x$anims, as.character)
    # Fill each list out with blanks
    maxLength <- max(sapply(animChar, length))
    animFull <- lapply(animChar,
                       function(x) {
                           if (length(x) < maxLength)
                               x <- c(x, rep("", maxLength - length(x)))
                           x
                       })
    animForm <- lapply(animFull, format)
    do.call(paste, c(animForm, list(sep="|")))
}

as.character.tracAnim <- function(x, ...) {
    unlist(lapply(x$anims, as.character))
}

print.vecAnim <- function(x, ...) {
    cat(as.character(x), sep="\n")
}

print.tracAnim <- function(x, ...) {
    cat(as.character(x), sep="\n")
}

