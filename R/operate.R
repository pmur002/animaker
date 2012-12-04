
# Operations on containers

"[.containerAnim" <- function(x, index, ...) {
    x$anims <- x$anims[index]
    if (length(x$start) > 1) {
        x$start <- x$start[index]
    }
    if (!is.null(x$durn) &&
        length(x$durn) > 1) {
        x$durn <- x$durn[index]
    }
    x
}

"[[.containerAnim" <- function(x, index, ...) {
    x$anims[[index]]
}

"[<-.containerAnim" <- function(x, index, value, ...) {
    x$anims[index] <- value$anims
    if (length(x$start) > 1) {
        if (length(value$start) > 1) {
            x$start[index] <- value$start
        } else {
            x$start[index] <- sapply(value$anims, start)
        }
    } else {
        if (length(value$start) > 1)
            warning("Dropping start information on assignment")
    }
    if (!is.null(x$durn) &&
        length(x$durn) > 1) {
        if (!is.null(value$durn) &&
            length(value$durn) > 1) {
            x$durn[index] <- value$durn
        } else {
            x$durn[index] <- sapply(value$anims, durn)
        }
    } else {
        if (!is.null(value$durn) &&
            length(value$durn) > 1)
            warning("Dropping durn information on assignment")
    }
    x
}

"[<-.vecAnim" <- function(x, index, value, ...) {
    if (!inherits(value, "vecAnim"))
        stop("Invalid value to assign")
    NextMethod()
}

"[<-.tracAnim" <- function(x, index, value, ...) {
    if (!inherits(value, "tracAnim"))
        stop("Invalid value to assign")
    NextMethod()
}

"[[<-.containerAnim" <- function(x, index, value, ...) {
    if (!inherits(value, "anim"))
        stop("Invalid value to assign")
    x$anims[[index]] <- value
    x
}

rep.containerAnim <- function(x, ...) {
    # Because children may have NA durns we need to enforce durns first
    # before repeating them. This is because we would expect that the
    # duration of an animation for one iteration to be repeated even if
    # it is not explicitly declared.
    nadurns <- any(is.na(sapply(x$anims, durn)))
    if (nadurns) {
        filledDurns <- durns(x)
        for (i in 1:length(x)) {
            durn(x$anims[[i]]) <- filledDurns[i]
        }
        if (!is.null(x$durn) && length(x$durn) == 1) {
            x$durn <- sum(rep(sum(filledDurns), ...))
        }
    }

    x$anims <- rep(x$anims, ...)
    if (length(x$start) > 1) {
        x$start <- rep(x$start, ...)
    }

    if (!is.null(x$durn) &&
        length(x$durn) > 1) {
        x$durn <- rep(x$durn, ...)
    }
    x
}

# Splicing can produce a variety of results
# 1. after=NULL, at=NULL means after=length(x) (i.e., append)
# 2. after=<n> means insert between <n> and <n+1> (i.e., append)
# 3. at=<n> means attach alongside <n>
#    (i)  x is vecAnim means splice(x[1:(n-1)], trac(value, x[n:length(x)]))
#    (ii) x is tracAnim means splice(x[1:(n-1)], vec(value, x[n:length(x)]))

checkSpliceArgs <- function(x, after, at) {
    if (!is.null(after) && !is.null(at)) {
        stop("Cannot specify both after and at")
    }
    if (!is.null(after) &&
        (after < 0 || after > length(x))) {
        stop("Invalid after")
    }
    if (!is.null(at) &&
        (at < 1 || at > length(x))) {
        stop("Invalid at")
    }
}

splice <- function(x, values, after=NULL, at=NULL) {
    UseMethod("splice")
}

appendAnim <- function(x, values, after=length(x)) {
    x$anims <- append(x$anims, values$anims, after)
    if (length(x$start) > 1) {
        if (length(values$start) > 1) {
            x$start <- append(x$start, values$start, after)
        } else {
            x$start <- append(x$start, sapply(values$anims, start), after)
        }
    } else {
        if (length(values$start) > 1) 
            warning("Dropping start information on assignment")
    }
    if (!is.null(x$durn) &&
        length(x$durn) > 1) {
        if (!is.null(values$durn) &&
            length(values$durn) > 1) {
            x$durn <- append(x$durn, values$durn, after)
        } else {
            x$durn <- append(x$durn, sapply(values$anims, durn), after)
        }
    } else {
        if (!is.null(values$durn) &&
            length(values$durn) > 1)
            warning("Dropping durn information on assignment")
    }
    x
}

splice.vecAnim <- function(x, values, after=NULL, at=NULL) {
    checkSpliceArgs(x, after, at)
    if (is.null(after) && is.null(at)) {
        after <- length(x)
    }
    if (is.null(at)) {
        if (!inherits(values, "vecAnim")) {
            if (inherits(values, "atomicAnim") ||
                inherits(values, "tracAnim")) {
                values <- vec(values)
            } else {
                stop("Invalid value to splice")
            }
        }
        appendAnim(x, values, after)
    } else {
        if (at == 1) {
            trac(values, x)
        } else {
            splice(x[1:(at-1)], trac(values, x[at:length(x)]))
        }
    }
}

splice.tracAnim <- function(x, values, after=NULL, at=NULL) {
    checkSpliceArgs(x, after, at)
    if (is.null(after) && is.null(at)) {
        after <- length(x)
    }
    if (is.null(at)) {
        if (!inherits(values, "tracAnim")) {
            if (inherits(values, "atomicAnim") ||
                inherits(values, "vecAnim")) {
                values <- trac(values)
            } else {
                stop("Invalid value to splice")
            }
        }
        appendAnim(x, values, after)
    } else {
        if (at == 1) {
            vec(values, x)
        } else {
            splice(x[1:(at-1)], vec(values, x[at:length(x)]))
        }
    }
}

