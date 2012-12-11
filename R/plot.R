
# Code to draw representations of animations

animScale <- function(start, durn, xscale) {
    if (is.null(xscale)) {
        # Special case of atomic animation that does not know its duration
        if (is.na(durn))
            durn <- 0
        if (durn == 0) {
            xscale <- c(start - 1, start + 1)
        } else {
            xscale <- c(0, start + durn)
        }
    }
    xscale
}

drawScale <- function(xscale) {
    times <- axisTicks(xscale, log=FALSE)
    grid.text(times, times, unit(-1, "lines"), default.units="native")
    grid.text(times, times,
              unit(1, "npc") + unit(1, "lines"), default.units="native")
    grid.circle(times, 0, default.units="native",
                r=unit(1, "mm"), gp=gpar(fill="black"))
    grid.circle(times, 1, default.units="native",
                r=unit(1, "mm"), gp=gpar(fill="black"))
}

pageSetup <- function(s, d, xscale) {
    grid.newpage()
    xscale <- animScale(s, d, xscale)
    pushViewport(viewport(width=.8, height=.8,
                          xscale=xscale))
    grid.rect(gp=gpar(col=NA, fill="grey95"))
    drawScale(xscale)
}

pageCleanup <- function() {
    upViewport()
}

drawAnim <- function(x, ...) {
    UseMethod("drawAnim")
}

drawAnim.atomicAnim <- function(x, s=start(x), d=durn(x), y=1,
                            newpage=TRUE, xscale=NULL,
                            margin=unit(5, "mm"), ...) {
    if (newpage) {
        pageSetup(s, d, xscale)
    }
    # Special case of atomic animation that does not know its duration
    if (is.na(d))
        d <- 0
    if (d == 0) {
        grid.segments(s, unit(y-1, "native") + margin,
                      s, unit(y, "native") - margin,
                      default.units="native",
                      gp=gpar(col=x$col))
    } else {
        grid.rect(s, y-.5, d,
                  unit(1, "native") - 2*margin, default.units="native",
                  just=c("left"),
                  gp=gpar(col=x$col, fill=adjustcolor(x$col, alpha.f=.5)))
    }
    if (newpage) {
        pageCleanup()
    }
}

drawAnim.vecAnim <- function(x, s=start(x), d=durn(x), y=1,
                             newpage=TRUE, xscale=NULL,
                             lwd=5, alpha=.5, 
                             margin=unit(5, "mm"), ...,
                             FUN=drawAnim) {
    if (newpage) {
        pageSetup(s, d, xscale)
    }
    starts <- starts(x)
    durns <- durns(x)
    nAnim <- length(x$anims)
    pushViewport(viewport(s, y-.5, d, 1,
                          default.units="native",
                          just=c("left"),
                          xscale=c(0, d))) # starts[nAnim] + durns[nAnim])))
    for (i in 1:nAnim) {
        FUN(x$anims[[i]], s=starts[i], d=durns[i], y=1,
            newpage=FALSE, lwd=.5*lwd, ...)
    }
    grid.segments(0:1, unit(0, "npc") + .5*margin,
                  0:1, unit(1, "npc") - .5*margin,
                  gp=gpar(col=rgb(0, 0, 0, alpha), lwd=lwd,
                      lineend="butt"))
    upViewport()
    if (newpage) {
        pageCleanup()
    }
}

drawAnim.tracAnim <- function(x, s=start(x), d=durn(x), y=1,
                              newpage=TRUE, xscale=NULL,
                              lwd=5, alpha=.5, 
                              margin=unit(5, "mm"), ...,
                              FUN=drawAnim) {
    if (newpage) {
        pageSetup(s, d, xscale)
    }
    starts <- starts(x)
    durns <- durns(x)
    nAnim <- length(x$anims)
    pushViewport(viewport(s, y-.5, d, 1, default.units="native",
                          just=c("left"),
                          xscale=c(0, d), # max(starts + durns)),
                          yscale=c(0, nAnim)))
    for (i in 1:nAnim) {
        FUN(x$anims[[i]], s=starts[i], d=durns[i], y=(nAnim - i + 1),
            newpage=FALSE, lwd=lwd, ...)
    }
    upViewport()
    if (newpage) {
        pageCleanup()
    }
}

plot.anim <- function(x, ...) {
    require(grid)
    drawAnim(x, ...)
}

dynDrawAnim <- function(x, ..., offset=0) {
    UseMethod("dynDrawAnim")
}

dynDrawAnim.atomicAnim <- function(x, s=start(x), d=durn(x), y=1,
                                   newpage=TRUE, xscale=NULL,
                                   timeUnit = c("s", "ms", "m"),
                                   margin=unit(5, "mm"), ..., offset=0) {
    if (newpage) {
        pageSetup(s, d, xscale)
    }
    if (d == 0) {
        grid.segments(s, unit(y-1, "native") + margin,
                      s, unit(y, "native") - margin,
                      gp=gpar(col=x$col))
    } else {
        rg <- rectGrob(s, y-.5, 0,
                       unit(1, "native") - 2*margin, default.units="native",
                       just=c("left"),
                       gp=gpar(col=x$col, fill=adjustcolor(x$col, alpha.f=.5)))
        rgg <- rectGrob(s, y-.5, d,
                        unit(1, "native") - 2*margin, default.units="native",
                        just=c("left"),
                        gp=gpar(col="grey", fill=NA))

        begin <- offset + s
        duration <- d
        # We might not be using seconds, in which case, convert to seconds
        timeUnit <- match.arg(timeUnit)
        if (timeUnit == "m") {
            begin <- begin * 60
            duration <- duration * 60
        } else if (timeUnit == "ms") {
            begin <- begin / 1000
            duration <- duration / 1000
        }

        rga <- animateGrob(rg, width=c(0, d),
                           begin=begin, duration=duration)
        grid.draw(rgg)
        grid.draw(rga)
    }
    if (newpage) {
        pageCleanup()
    }
}

dynDrawAnim.containerAnim <- function(x, s=start(x), ..., offset=0) {
    drawAnim(x, s=s, ..., offset=s + offset, FUN=dynDrawAnim)
}

dynPlot <- function(x, file="anim.svg", ...) {
    require(gridSVG)
    dynDrawAnim(x, ...)
    gridToSVG(file)
}
