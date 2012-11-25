
# For this prototype system, actions are just colours
# An animation will be visualised by drawing rectangles with
# different colours
colourPal <- function() {
    # Global palette; size appropriately to cover total number of anims
    Ncol <- 10
    palette <- hcl(base::seq(0, 360, length=Ncol+1)[-1], 60, 60)
    whichCol <- 0
    function() {
        # Rotate through palette
        whichCol <<- whichCol + 1
        if (whichCol > Ncol)
            whichCol <<- 1
        palette[whichCol]
    }
}
colourSel <- colourPal()

