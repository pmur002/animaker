
# The simplest component of an animation is an atomic animation
# This consists of an action, a start (default 0), and a duration (default 0)
# Give it a label too so there's some way to keep track/visualise them

atomic <- function(start=0, durn=0, label=labelSel()) {
    x <- list(start=start, durn=durn,
              label=label, col=colourSel())
    class(x) <- c("atomicAnim", "anim")
    x
}

start <- function(x) {
    UseMethod("start")
}

start.atomicAnim <- function(x) {
    x$start
}

durn <- function(x) {
    UseMethod("durn")
}

durn.atomicAnim <- function(x) {
    x$durn
}

"durn<-.atomicAnim" <- function(x, value) {
    x$durn <- value
    x
}

as.character.atomicAnim <- function(x, ...) {
    paste(x$label, x$start, x$durn, sep=":")
}

length.atomicAnim <- function(x) {
    1
}

# A bit of a special case
# atomicAnims are *scalar*, so a rep()
# generates a seqAnim or trackAnim (a containerAnim)
rep.atomicAnim <- function(x, ..., vec=TRUE) {
    if (vec) {
        do.call("vec", rep(list(x), ...))
    } else {
        do.call("trac", rep(list(x), ...))        
    }
}

print.anim <- function(x, ...) {
    cat(as.character(x), sep="\n")
}

