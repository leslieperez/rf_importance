delta <- 1

getScore <- function(pname, value, config, le) {
  if (!le$isActive(pname, config)) 
    return(NULL)
  
  x <- config
  x[pname] <- value
  x <- le$checkConfig(x)
  
  if (pname %in% names(le$conditional)) {
    all.score <- c()
    # other parameters depend on pname 
    for (pcond in le$conditional[[pname]]) {
      if (le$isActive(pcond, x)) {
        pvalues <- le$sampleUnif(pcond,10)
        pscore <- c()
        for (pval in pvalues){
          pscore <- c(pscore, getScore(pcond, pval, x, le) )
        }
        all.score <- c(all.score, max(pscore))
      }
    }
    if (length(all.score) < 1) {
      score <- le$getEval(config = x) 
    } else {
      score <- max(all.score)
    }
  } else {
    score <- le$getEval(config = x) 
  }
  return(score)
}

isLocallyImportant <- function(pname, config, le) {
  #pdomain <- as.numeric(le$parameters$domain[[pname]])
  #cat("Evaluating importance of:", pname, "\n")
  
  # Not active parameters are not locally important
  if (!le$isActive(pname, config)) {
    return(FALSE)
  }
  
  current.eval <- le$getEval(config)
  other.eval <- c()
  values <- le$sampleUnif(pname, 10)
  
  for(v in values[!(values %in% config[pname])]){
    new.eval <- getScore(pname, v, config, le)
    other.eval <- c(other.eval, new.eval)
  }
  
  if (any(abs(current.eval- other.eval) > delta))
    return(TRUE)
  return(FALSE)
} 