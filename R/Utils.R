
getIndexValuesInRange = function(x, rang){
  which( (rang[[1]] <= x) & (x <= rang[[2]]) )
}


VerboseMessage = function(verbose, msg, symbol="", tab=""){
  if (verbose) {
    message(paste0(tab, symbol, " ", msg, " ", symbol))
  }
}

# If new is NULL -> there is no new argument replacing the old one, we ignore
# the argument. In other case we recommend to use the new one
DeprecatedArgMessage = function(old, new = NULL) {
  if (is.null(new)) {
    return(paste0(old, ": deprecated argument ignored."))
  } else{
    return(paste0(old, ": deprecated argument. Using ",new," instead."))
  }
}

MissingNonLinearObjectMessage = function(missing, use_before, use_after) {
  paste0(missing," not found! Use ",
         use_before, " before using ", 
         use_after,"!")
}

# format numbers for VerboseMessage
rhrvFormat = function(x){
  kNDIGITS = 4
  round(x, digits = kNDIGITS)
}