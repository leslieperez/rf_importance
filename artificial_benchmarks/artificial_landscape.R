library("irace")
library("R6")

Landscape <- R6Class("Landscape",
                   public = list(
                     parameters = NULL,
                     
                     pnames = c(),
                     
                     base = c(), 
                     
                     weight = c(),
                     
                     target = list(),
                     
                     interaction = list(),
                     
                     conditional = list(),
                     
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
                         for(d in dep) {
                           self$conditional[[d]] <- unique(c(self$conditional[[d]], pname))
                         }
                       }
                     }, 
                     
                     inTarget = function (value, pname) {
                       if(self$parameters$types[pname] == "c") {
                         aux <-  value %in% self$target[[pname]]
                       } else {
                         aux <- value >= self$target[[pname]][1] &&  value <= self$target[[pname]][2]
                       }
                       return(aux)
                     },
                     
                     hasInteractions = function (pname) {
                       if (is.na(self$interaction[[pname]])) 
                         return(FALSE)
                       else
                         return(TRUE)
                     },
                     
                     partialEval = function(config, pname) {
                       # If parameter is not active we return the base value
                       if (!self$isActive(pname, config))
                         return(0)
                       
                       value <- config[pname]
                       current.in.target <- self$inTarget(value=value, pname=pname)
                       fx <- self$base[pname]
                       if (self$hasInteractions(pname)) {
                         iparam <- self$interaction[[pname]]
                         ivalue <- config[iparam]
                         if (current.in.target && self$inTarget(value=ivalue, pname=iparam)) {
                           fx <- (fx * self$weight[pname])
                         }
                       } else if (current.in.target) {
                         fx <- (fx * self$weight[pname])
                       }
                       return (fx)
                     },
                     
                     getEval = function (config) {
                       fx <- 0
                       for (pname in self$pnames) {
                         if (self$isActive(pname, config)) {
                           fx <- fx + self$partialEval(config, pname)
                         }
                       }
                       return(fx)
                     },
                     
                     getPartialEval = function (config) {
                       fx <- c()
                       for (pname in self$pnames) {
                         fx <- c(fx , self$partialEval(config, pname))
                       }
                       return(fx)
                     },
                     
                     isActive = function (pname, config)  {
                       condition <- self$parameters$conditions[[pname]]
                       if (isTRUE(condition)) 
                         return(TRUE)
                       v <- eval(condition, as.list(config))
                       v <- !is.na(v) && v
                       return(v)
                     },
                     
                     sampleUnif = function (pname, n) {
                       ptype <- self$parameters$types[pname]
                       pdomain <- as.numeric(self$parameters$domain[[pname]])
                       if (ptype=="i") {
                         values <- unique(c(round(runif(n, min=pdomain[1], max=pdomain[2])), pdomain))
                       } else if (ptype == "r") {
                         values <- unique(c(runif(n, min=pdomain[1], max=pdomain[2]), pdomain))
                       } else {
                         values <- pdomain
                       }
                       return(values)
                     },
                     
                     checkConfig = function (config) {
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






