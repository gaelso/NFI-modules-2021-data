## NFI-modules-2021-learnr
## Gael Sola, FAO

## Remove cat() calls from functions
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
