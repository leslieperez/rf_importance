delta <- 1

getScore <- function(pname, value, config, le) {
  if (!le$isActive(pname, config)) 
    return(NULL)
  
  x <- config
  x[pname] <- value
  x <- le$deactivateConfig(x)
  
  if (pname %in% names(le$conditional)) {
    all.score <- c()
    # other parameters depend on pname 
    for (pcond in le$conditional[[pname]]) {
      if (le$isActive(pcond, x)) {
        pvalues <- le$getValuesToEvaluate(pcond,10)
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
  values <- le$getValuesToEvaluate(pname, 10)
  
  for(v in values[!(values %in% config[pname])]){
    new.eval <- getScore(pname, v, config, le)
    other.eval <- c(other.eval, new.eval)
  }
  
  if (any(abs(current.eval- other.eval) > delta))
    return(TRUE)
  return(FALSE)
} 


globalImportance <- function(le){
  x <- rep(NA, length(le$pnames))
  names(x) <- le$pnames
  
  domains <- list()
  for (pname in le$parameters$names) {
    domains[[pname]] <- le$getValuesToEvaluate(pname = pname, n=3)
  }

  configs <- listConfigurations(current=x, configs=NULL, domains=domains, pnames=le$parameters$names, le=le)
  
  nconf <- length(configs)
  important <- rep(NA, length(le$pnames))
  names(important) <- le$pnames
  for(pname in le$parameters$names) {
    imp <- sapply(configs, isLocallyImportant, pname=pname, le=le)
    important[pname]<- sum(imp)
  }
  return(important)
}

listConfigurations <- function(current, configs, domains, pnames, le) {
  if(is.null(configs))
    configs <- list()
  
  if (length(pnames) <1) {
    configs[[length(configs)+1]] <- current
    return(configs)
  }

  pname <- pnames[1]
  
  if (!le$isActive(pname, current)) {
    current[pname] <- NA
    new.names <- pnames[!(pnames %in% pname)]
    configs <-listConfigurations(current=current, configs=configs, domains=domains, pnames=new.names, le=le)
  } else {
    for(v in domains[[pname]]) {
      current[pname] <- v
      new.names <- pnames[!(pnames %in% pname)]
      configs <- listConfigurations(current=current, configs=configs, domains=domains, pnames=new.names, le=le)
    }
  }
  return(configs)
}