library("irace")
library("R6")

#' Landscape Class implementing a benchmark 
#'
#' @description
#' The landscape has an irace parameter object associated. 
#' Things to consider:
#' \describe{
#'   \item{}{This implementation considers a maximization problem}
#'   \item{}{Conditional parameters do not contribute to the objective function value if they are not active}
#' }
#' The response of the objective function is determined by the contribution 
#' of each parameter defined by:
#' \describe{
#'   \item{parameters:}{a parameter space definition in the format user by irace 
#'    (readParameters function)}
#'   \item{base:}{a base contribution of each parameter. Non active parameters
#'    do not contribute to the objective value}
#'   \item{weight:}{a weight to increment/decrease the contibution of a parameter 
#'    when a target value us set}
#'   \item{target:}{a set of target values (set for categorical and ordinal parameters,
#'    a range within its domainfor numerical parameters)}
#'   \item{conditional:}{a list that gives the parameters that are activated by the
#'    key parameter}
#'   \item{interaction:}{a list that conditionates the contibution of the key parameter
#'    based on another parameter}
#' }
#'
#' @details
#' This class provides methods to evaluate configurations
Landscape <- R6Class("Landscape",
                   public = list(
                     #' @field parameters parameter definition object (irace format)
                     parameters = NULL,
                     #' @field pnames Vector of parameter names
                     pnames = c(),
                     #' @field rnames Vector of root parameter names
                     rnames = c(),
                     #' @field base Vector of base contributions of the parameters  
                     base = c(), 
                     #' @field weight Vector of weights of the contributions of the parameters  
                     weight = c(),
                     #' @field target List of the target values of the parameters (set for categorical 
                     #' and ordered parameters and range for numerical)  
                     target = list(),
                     #' @field interaction List of the interactions of contributions of the parameters  
                     interaction = list(),
                     #' @field conditional List of conditional dependency of the parameters  
                     conditional = list(),
                     
                     #' @description
                     #' Create a new landscape object
                     #' @param parameter_file String of the path to a parameter definition file
                     #' @param landscape_file String of the path to a landscape definition file
                     #' @return A new `Landscape` object.
                     initialize = function (parameter_file, landscape_file) {
                       file.check <- irace:::file.check
                       is.wholenumber <- irace:::is.wholenumber
                       trim <- irace:::trim
                       
                       print("# Reading landscape ...")
                       
                       self$parameters <- irace::readParameters(file = parameter_file, digits = 4)
                       
                       irace:::file.check(landscape_file, readable = TRUE, text = "landscape file")
                       lines <- readLines(con = landscape_file)
                       
                       pcount <- 0
                       
                       for (line in lines) {
                         line <- trim(sub("#.*$", "", line))
                         if (nchar(line) == 0) {
                           next
                         }
                         
                         pcount <- pcount + 1
                         # parameter name
                         result <- private$field.match (line, "[._[:alnum:]]+")
                         param.name <- result$match
                         line <- result$line
                         #cat("# Reading parameter: ", param.name, "...\n")
                         
                         self$pnames <- c(self$pnames, param.name)
                         param.type <- self$parameters$types[param.name]
                         
                         # parameter base 
                         result <- private$field.match (line, "[._[:digit:]]+")
                         param.base <- suppressWarnings(as.numeric(result$match))
                         line <- result$line
                         
                         # parameter weight
                         result <- private$field.match (line, "[._[:digit:]]+")
                         param.weight <- suppressWarnings(as.numeric(result$match))
                         line <- result$line
                         
                         # parameter target for activation
                         result <- private$field.match (line, "\\([^|]+\\)", delimited = TRUE, sep = "")
                         param.value <- result$match
                         line <- result$line
                         
                         param.value <- private$string2vector(param.value)
                         
                         if (param.type %in% c("i", "r")) {
                           param.value <- suppressWarnings(as.numeric(param.value))
                         }
                         
                         # parameter interactions (activation depends on other parameters)
                         result <- private$field.match (line, "\\|", sep="")
                         line <- result$line
                         if (!is.null(result$match) && nchar(result$match)) {
                           result <- private$field.match (line, ".*$", sep="")
                           line <- result$line
                           self$interaction[[param.name]] <- result$match
                           line <- result$line
                         } else{
                           self$interaction[[param.name]] <- NA
                         }
                         self$target[[param.name]] <- param.value
                         self$weight <- c(self$weight, param.weight)
                         self$base <- c(self$base, param.base)
                       }
                       private$assert(all(self$pnames %in% self$parameters$names))
                       names(self$base) <- names(self$weight) <- self$pnames
                       
                       # make available conditionality
                       for (pname in self$pnames) {
                         dep <- self$parameters$depends[[pname]]
                         if (length(dep)<1) self$rnames <- c(self$rnames, pname)
                         for(d in dep) {
                           self$conditional[[d]] <- unique(c(self$conditional[[d]], pname))
                         }
                       }
                     }, 
                     
                     #' @description
                     #' Checks if a parameter value is in the target defined in landscape 
                     #' @param value Parameter value to check
                     #' @param pname Parameter name for which parameter value should be checked
                     inTargetParam = function (value, pname) {
                       if(self$parameters$types[pname] == "c") {
                         aux <-  value %in% self$target[[pname]]
                       } else {
                         aux <- value >= self$target[[pname]][1] &&  value <= self$target[[pname]][2]
                       }
                       return(aux)
                     },
                     
                     #' @description
                     #' Checks if a parameter value is in the target together with its interacting parameters
                     #' @param pname Parameter name for which parameter value should be checked
                     #' @param config Configuration to be evaluated
                     inTargetAll = function(pname, config) {
                       current.in.target <- self$inTargetParam(value=config[pname], pname=pname)
                       if (!current.in.target) 
                         return(FALSE)
                       if (self$hasInteractions(pname)) {
                         iname <- self$interaction[[pname]]
                         ivalue <- config[iname]
                         current.in.target <- current.in.target && self$inTargetParam(ivalue, iname)
                       }
                       return(current.in.target)
                     },
                     
                     #' @description
                     #' Checks if a parameter has interactions in the landscape
                     #' @param pname Parameter name of the parameter to be checked
                     hasInteractions = function (pname) {
                       if (is.na(self$interaction[[pname]])) 
                         return(FALSE)
                       else
                         return(TRUE)
                     },
                     
                     #' @description
                     #' Evaluates the contribution of  a parameter to the objective function  
                     #' @param config Configuration to be evaluated
                     #' @param pname Parameter name for which contribution should be evaluated
                     partialEval = function(config, pname) {
                       # If parameter is not active we return the base value
                       if (!self$isActive(pname, config))
                         return(0)
                       
                       value <- config[pname]
                       current.in.target <- self$inTargetAll(pname=pname, config=config)
                       fx <- self$base[pname]
                       if (current.in.target) {
                         fx <- (fx * self$getWeight(pname=pname, config=config))
                       } 
                       return(fx)
                     },
                     
                     #' @description
                     #' Evaluates a configuration on the objective function described by the landscape 
                     #' @param config Configuration to be evaluated
                     getEval = function (config) {
                       fx <- 0
                       # sum the contribution of each root parameter
                       for (pname in self$rnames) {
                         fx <- fx + self$partialEval(config, pname)
                       }
                       return(fx)
                     },
                     
                     #' @description
                     #' Gets a Vector with the partial evaluations of all parameters in a configuration
                     #' @param config Configuration to be evaluated
                     getPartialEval = function (config) {
                       fx <- c()
                       for (pname in self$pnames) {
                         fx <- c(fx , self$partialEval(config, pname))
                       }
                       return(fx)
                     },
                     
                     #' @description
                     #' Checks if a parameter is active in a configuration
                     #' @param config Configuration to be checked
                     #' @param pname Parameter name that should be checked
                     isActive = function (pname, config)  {
                       condition <- self$parameters$conditions[[pname]]
                       if (isTRUE(condition)) 
                         return(TRUE)
                       v <- eval(condition, as.list(config))
                       v <- !is.na(v) && v
                       return(v)
                     },
                     
                     #' @description
                     #' Generate a set of parameter values to evaluate a parameter. If the parameter is 
                     #' categorical, the values correspond to the full domain. If the parameter is numerical
                     #' the values are sampled uniformly at random and they include the upper and lower bound of the
                     #' domain
                     #' @param pname Parameter name for which contribution should be evaluated
                     #' @param n Number of values to be obtained
                     getValuesToEvaluate = function (pname, n) {
                       ptype <- self$parameters$types[pname]
                       pdomain <- as.numeric(self$parameters$domain[[pname]])
                       if (ptype=="i") {
                         values <- unique(c(round(runif(n-2, min=pdomain[1], max=pdomain[2])), pdomain))
                       } else if (ptype == "r") {
                         values <- unique(c(runif(n-2, min=pdomain[1], max=pdomain[2]), pdomain))
                       } else {
                         values <- pdomain
                       }
                       return(values)
                     },
                     
                     #' @description
                     #' Sets as NA  all non active parameters
                     #' @param config Configuration to be corrected
                     deactivateConfig = function (config) {
                       for (pname in self$pnames) {
                         if(!self$isActive(pname, config)) 
                           config[pname] <- NA
                       }
                       return(config)
                     },
                     
                     print = function () {
                       cat("# Parameters in landscape: ", length(self$pnames), "\n")
                       for (pname in self$pnames) {
                         cat("# Name: ", pname, "\n")
                         cat("#   type: ", self$parameters$types[pname], "\n")
                         cat("#   base: ", self$base[pname], "\n")
                         cat("#   weight: ", self$weight[pname], "\n")
                         cat("#   interaction: ", self$interaction[[pname]], "\n")
                         cat("#   target: ", self$target[[pname]][1])
                         if (length(self$target[[pname]])>1) {
                           for (i in 2:length(self$target[[pname]])) 
                              cat(", ",self$target[[pname]][i])
                         }
                         cat("\n")
                       }
                     },
                     
                     #' @description
                     #' Checks if a parameter activates conditional parameters 
                     #' @param pname Parameter value to check
                     hasConditionals = function(pname) {
                       if (pname %in% names(self$conditional))
                         return(TRUE)
                       return(FALSE)
                     },
                     
                     getWeight = function(pname, config) {
                       # parameter is not active
                       if (!self$isActive(pname, config))
                         return(0)
                       
                       # check if the parameter defines conditional parameters
                       if (!self$hasConditionals(pname)) {
                         # parameter does not have conditionals
                         if (self$inTargetAll(pname=pname, config=config)) {
                           return(self$weight[pname])
                         }
                         return(1)
                       } else {
                         w <- 1;
                         for (cname in self$conditional[[pname]]) {
                           if (self$isActive(cname, config))
                              w <- w * self$getWeight(cname, config)   
                         }
                         return(w)
                       }
                     }
                   ),
                   private = list(
                     field.match = function (line, pattern, delimited = FALSE, sep = "[[:space:]]") {
                       trim.leading <- irace:::trim.leading
                       trim <- irace:::trim
                       #cat ("pattern:", pattern, "\n")
                       positions <- lapply(1:length(pattern), function(x) regexpr (paste0("^", pattern[x], sep), line))
                       if (all(sapply(positions, "[[", 1) == -1)) {
                         #cat("no match: NULL\n")
                         return (list(match = NULL, line = line))
                       }
                       pos.matched.list <- lapply(1:length(pattern), function(x) regexpr (paste0("^", pattern[x]), line))
                       #cat("pos.matched:", pos.matched, "\n")
                       if (all(sapply(pos.matched.list, "[[", 1) == -1)) {
                         #cat(line)
                         return (list(match = NULL, line = line))
                       }
                       position <- which(sapply(pos.matched.list, `[[`,1) != -1)
                       if (length(position) > 1) {
                         position <- position[1]
                       }
                       pos.matched <- pos.matched.list[[position]]
                       delimited <- as.integer(delimited)
                       match <- substr(line, pos.matched[1] + delimited,
                                       attr(pos.matched, "match.length") - delimited)
                       #cat("match:",match, "\n")
                       line <- substr (line, pos.matched[1] + attr(pos.matched, "match.length"),
                                       nchar(line))
                       line <- trim.leading (line)
                       #cat(line)
                       return (list(match = match, line = line))
                     },
                     
                     string2vector = function(str) {
                       trim <- irace:::trim
                       v <- c()
                       str <- trim(str)
                       #cat("string2vector:", str, "\n")
                       while (nchar (str)) {
                         result <- private$field.match (str, "\"[^\"]*\"", delimited = TRUE, sep="")
                         #cat("result.match: ", result$match,"\n")
                         if (is.null (result$match)) {
                           result <- private$field.match (str, "[^,]+", sep="")
                           #cat("result.match: ", result$match,"\n")
                         }
                         v <- c(v, result$match)
                         #print(v)
                         str <- sub(",[[:space:]]*", "", result$line)
                         #print(str)
                       }
                       return (v)
                     },
                     
                     assert = function (exp, eval.after = NULL) {
                       if (exp) 
                         return(invisible())
                       mc <- match.call()[[2]]
                       msg <- paste0(deparse(mc), " is not TRUE\n")
                       if (!is.null(eval.after)) {
                         msg.after <- eval.parent(capture.output(eval.after))
                         msg <- paste0(msg, "\n", msg.after)
                       }
                       print(msg)
                       invisible()
                       stop()
                     }
                     
                   )
)






