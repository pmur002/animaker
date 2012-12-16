# Someone might try to use a JS keyword, so stop them from using it.
# List of keywords taken from:
# https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
keywords <-
    c("break", "do", "instanceof", "typeof", "case", "else", "new", "var",
      "catch", "finally", "return", "void", "continue", "for", "switch",
      "while", "debugger", "function", "this", "with", "default", "if",
      "throw", "delete", "in", "try", 
      # future reserved words
      "class", "enum", "extends", "super", "const", "export", "import",
      "implements", "let", "private", "public", "interface", "package",
      "protected", "static", "yield")

export <- function(x, jsVar = "timingData", file = "timingData.js") {
    if (! require(RJSONIO))
        stop("the RJSONIO package is required for exporting")
    if (jsVar %in% keywords)
        stop("JS variable is a JS keyword")
    UseMethod("export")
}

export.timing <- function(x, jsVar = "timingData", file = "timingData.js") {
    timingJS <- paste("var ", jsVar, " = ",
                      toJSON(x), ";", sep = "")
    if (is.null(file) || ! nzchar(file))
        timingJS
    else
        cat(timingJS, "\n", file = file)
}

export.anim <- function(x, jsVar = "timingData", file = "timingData.js") {
    export(timing(x), jsVar, file)
}
