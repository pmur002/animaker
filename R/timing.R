
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

timing <- function(x, ...) {
    UseMethod("timing")
}

timing.atomicAnim <- function(x,
                              s=start(x), d=durn(x), lab=x$label,
                              vec=NULL, vecNum=NULL,
                              trac=NULL, tracNum=NULL,
                              offset=0) {
    timingInfo(x, offset + s, d, lab, vec, vecNum, trac, tracNum)
}

timing.vecAnim <- function(x,
                           s=start(x), d=durn(x), lab=x$label,
                           vec=NULL, vecNum=NULL,
                           trac=NULL, tracNum=NULL,
                           offset=0) {
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
                            offset=0) {
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

