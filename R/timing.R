
# Generate a set of timing information from an "anim"

timingInfo <- function(x,
                       s=start(x), d=durn(x), lab=x$label,
                       vec=NULL, vecNum=NULL,
                       trac=NULL, tracNum=NULL) {
    info <- list(start=s, durn=d, label=lab,
                 vec=vec, vecNum=vecNum,
                 trac=trac, tracNum=tracNum)
    class(info) <- "timing"
    info
}

simplify <- function(x) {
    UseMethod("simplify")
}

simplify.timing <- function(x) {
    list(x)
}

simplify.timingList <- function(x) {
    x
}

timingInfoList <- function(x) {
    flat <- do.call("c", lapply(x, simplify))
    class(flat) <- c("timingList", "timing")
    flat
}

prepare <- function(x) {
    UseMethod("prepare")
}

"[.timingList" <- function(x, index, ...) {
    subset <- unclass(x)[index, ...]
    class(subset) <- c("timingList", "timing")
    subset
}

prepare.timing <- function(x) {
    basic <- as.data.frame(x[c("label", "start", "durn")],
                           stringsAsFactors=FALSE)
    context <- lapply(x[c("vec", "vecNum", "trac", "tracNum")],
                      paste, collapse=":")
    cbind(basic, as.data.frame(context, stringsAsFactors=FALSE))
}

prepare.timingList <- function(x) {
    do.call(rbind, lapply(x, prepare))
}

print.timing <- function(x, ...) {
    print(prepare(x), right=FALSE)
}

# Declarative action-based timing
timing <- function(x, ...) {
    UseMethod("timing")
}

timing.atomicAnim <- function(x,
                              s=start(x), d=durn(x), lab=x$label,
                              vec=NULL, vecNum=NULL,
                              trac=NULL, tracNum=NULL,
                              offset=0, ...) {
    timingInfo(x, offset + s, d, lab, vec, vecNum, trac, tracNum)
}

timing.vecAnim <- function(x,
                           s=start(x), d=durn(x), lab=x$label,
                           vec=NULL, vecNum=NULL,
                           trac=NULL, tracNum=NULL,
                           offset=0, ...) {
    starts <- starts(x)
    durns <- durns(x)
    nAnim <- length(x$anims)
    timings <- vector("list", nAnim)
    for (i in 1:nAnim) {
        timings[[i]] <- timing(x$anims[[i]],
                               s=starts[i],
                               d=durns[i],
                               vec=c(vec, x$label),
                               vecNum=c(vecNum, i),
                               trac=trac,
                               tracNum=tracNum,
                               offset=s + offset)
    }
    timingInfoList(timings)
}

timing.tracAnim <- function(x,
                            s=start(x), d=durn(x), lab=x$label,
                            vec=NULL, vecNum=NULL,
                            trac=NULL, tracNum=NULL,
                            offset=0, ...) {
    starts <- starts(x)
    durns <- durns(x)
    nAnim <- length(x$anims)
    timings <- vector("list", nAnim)
    for (i in 1:nAnim) {
        timings[[i]] <- timing(x$anims[[i]],
                               s=starts[i],
                               d=durns[i],
                               vec=vec,
                               vecNum=vecNum,
                               trac=c(trac, x$label),
                               tracNum=c(tracNum, i),
                               offset=s + offset)
    }
    timingInfoList(timings)
}

# Procedural frame-based timing information
frameTiming <- function(x, time=0) {
    timing <- timing(x)
    starts <- sapply(timing, "[[", "start")
    ends <- starts + sapply(timing, "[[", "durn")
    index <- (time >= starts) & (time < ends)
    timing[index]
}

frameApply <- function(x, FUN=print, fps=1, pause=TRUE) {
    if (fps < 1) 
        stop("Frames per second less than 1")
    increment <- 1/fps
    times <- seq(0, durn(x), increment)
    timing <- timing(x)
    starts <- sapply(timing, "[[", "start")
    ends <- starts + sapply(timing, "[[", "durn")
    for (i in times[-length(times)]) {
        index <- (i >= starts) & (i < ends)
        FUN(timing[index])
        if (pause)
            Sys.sleep(increment)
    }
    invisible()
}
