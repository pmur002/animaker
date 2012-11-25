
# A container animation is an object that can have a tree of children
# Each child may be either an atomic animation or a container animation
# Give it a label too so there's some way to keep track/visualise them

validateContainer <- function(x) {
    if (length(x$anims) &&
        !all(sapply(x$anims, inherits, "anim")))
        stop("Invalid contents of container")
    # start has to be numeric and either length 1 or length(anims)
    if (!(is.numeric(x$start) &&
          (length(x$start) == 1 || length(x$start) == length(x$anims))))
        stop("Invalid start of container")
    # durn has to be NULL or numeric and either length 1 or length(anims)
    if (!(is.null(x$durn) ||
          (is.numeric(x$durn) &&
           (length(x$durn) == 1 || length(x$durn) == length(x$anims)))))
        stop("Invalid durn of container")
}

container <- function(anims, start=0, durn=NULL, label=labelSel()) {
    x <- list(anims=anims, start=start, durn=durn,
              label=label, col=colourSel())
    validateContainer(x)
    class(x) <- c("containerAnim", "anim")
    x
}

length.containerAnim <- function(x) {
    length(x$anims)
}

print.containerAnim <- function(x, ...) {
    print(lapply(x$anims, as.character))
}



