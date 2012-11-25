
# Labels.txt copied from http://www.osric.com/chris/phonetic.html

# Autogeneration of labels
labelGen <- function() {
    labelSet <- c("Alpha", "Bravo", "Charlie", "Delta", "Echo",
                  "Foxtrot", "Golf", "Hotel", "India", "Juliet",
                  "Kilo", "Lima", "Mike", "November", "Oscar",
                  "Papa", "Quebec", "Romeo", "Sierra", "Tango",
                  "Uniform", "Victor", "Whiskey", "Xray", "Yankee", "Zulu")
    nLab <- length(labelSet)
    whichLab <- 0
    labelGet <- function() {
        # Rotate through label set
        whichLab <<- whichLab + 1
        if (whichLab > nLab)
            whichLab <<- 1
        labelSet[whichLab]
    }
    labelReset <- function() {
        whichLab <<- 0
    }
    list(labelGet=labelGet, labelReset=labelReset)
}
labelFuns <- labelGen()
labelSel <- labelFuns$labelGet
labelZero <- labelFuns$labelReset

