evalqOnLoad({
  # Load Judgment Modeules 
  loadModule("AdhocQrels", TRUE)
  loadModule("DivQrels", TRUE)
  loadModule("PrfJudgments", TRUE)
  
  # Load Utility Modules
  loadModule("Runs", TRUE)
})

# Generic Methods
num_ret <- function(grades, ...) UseMethod("num_ret")