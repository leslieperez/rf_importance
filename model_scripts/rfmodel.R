require("randomForest")
require("randomForestExplainer")
require("R6")
# Vignettes de randomForestExplainer
# https://cran.r-project.org/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
# Thesis that explains the methods in randomForestExplainer
# https://rawgit.com/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer_Master_thesis.pdf

#library("R6")

RFModel <- R6Class("RFModel",
                   public = list(
                     # model is an object obtained using the randomForest
                     # package
                     model = NULL,
                     # interactions_frame is a data frame obtained with the randomForestExplainer
                     # package, it contains the following columns:
                     # * variable: name of the dependent parameter
                     # * root_variable: name of the root parameter of the dependent parameter
                     # * mean_min_depth: indicator calculated based on the min depth value
                     #    obtained from the depth of the maximum tree in which splits on the  
                     #    two parameters is performed having root_variable as a root of that tree
                     # * ocurrences: number of time that a split on root_variable was followed (in 
                     #    any depth) by a split on the variable parameter
                     # * interaction: string that represents the interaction, that is root_variable:variable
                     # * uncond_mean_min_depth
                     interactions_frame = NULL,
                     full_interactions_frame = NULL,
                     
                     # order in which the parameters should be sampled if considered the current 
                     # interactions in interactions_frame
                     sampling.order = c(),
                     
                     # full training data used in the model
                     training_data = NULL,
                     
                     initialize = function(n_trees, parameters, scenario){
                       private$n_trees = n_trees
                       private$parameters = parameters
                       private$id_seed = scenario$seed
                       self$sampling.order = names(scenario$parameters$conditions)
                     },
                     
                     # function train model trains, calculate important parameters and
                     # generates a data frame with their interactions
                     trainModel = function(configurations, experiments) {
                       #library("randomForest")
                       #library("randomForestExplainer")
                       # create, filter and impute training data
                       data <- private$createData (configurations, experiments, add.dummy = TRUE)
                       
                       if (is.null(data)) {
                         cat ("# Warning: data provided by irace is does not allow to train the random ",
                              "forest model\n")
                         return (FALSE)
                       }
                       
                       # train the model
                       cat("# training general random forest model ...\n")
                       self$model <- randomForest::randomForest(x=data$data[,data$pnames], y=data$data$.PERFORMANCE., 
                                                                importance=TRUE, localImp = TRUE, ntree=private$n_trees)
                       self$training_data = data
                       
                       cat("# identifying important parameters ...\n")
                       private$identifyImportantParameters(configurations, experiments, force.dummy=TRUE)
                       
                       if (length(private$important_parameters)>1) {
                         # retrain the model
                         cat("# re-training model with important parameters ...\n")
                         self$model <- randomForest::randomForest(x=data$data[,private$important_parameters,drop=FALSE], 
                                                                  y=data$data$.PERFORMANCE., importance=TRUE, localImp = TRUE, 
                                                                  ntree=private$n_trees)                       
                       }
                       cat("# calculating interactions ...\n")
                       private$calculateInteractions()
                       
                       return (TRUE)
                     },
                     
                     # function train model trains, calculate important parameters and
                     # generates a data frame with their interactions
                     trainModelRank = function(configurations, experiments) {
                       # create, filter and impute training data
                       data <- private$createDataRank(configurations, experiments, add.dummy = TRUE)
                       
                       if (is.null(data)) {
                         cat ("# Warning: data provided by irace is does not allow to train the random ",
                              "forest model\n")
                         return (FALSE)
                       }
                       
                       # train the model
                       cat("# training general random forest model ...\n")
                       self$model <- randomForest::randomForest(x=data$data[,data$pnames], y=data$data$.PERFORMANCE., 
                                                                importance=TRUE, localImp = TRUE, ntree=private$n_trees)
                       self$training_data = data
                       
                       cat("# identifying important parameters ...\n")
                       private$identifyImportantParameters(configurations, experiments, force.dummy=TRUE)
                       
                       if (length(private$important_parameters)>1) {
                         # retrain the model
                         cat("# re-training model with important parameters ...\n")
                         self$model <- randomForest::randomForest(x=data$data[,private$important_parameters,drop=FALSE], 
                                                                  y=data$data$.PERFORMANCE., importance=TRUE, localImp = TRUE, 
                                                                  ntree=private$n_trees)                       
                       }
                       cat("# calculating interactions ...\n")
                       private$calculateInteractions()
                       
                       return (TRUE)
                     },
                     
                     # function train model trains, calculate important parameters and
                     # generates a data frame with their interactions
                     trainModelInstances = function(configurations, experiments) {
                       filtered = private$filterFewInstancesConfigurations(configurations, experiments)
                       experiments = filtered$experiments
                       configurations = filtered$configurations
                       
                       data <- private$createDataInstance(configurations, experiments, add.dummy = TRUE)
                       
                       if (is.null(data)) {
                         cat ("# Warning: data provided by irace is does not allow to train the random ",
                              "forest model\n")
                         return (FALSE)
                       }

                       # train the model
                       cat("# training general random forest model ...\n")
                       self$model <- randomForest::randomForest(x=data$data[,data$pnames], y=data$data$.PERFORMANCE., 
                                                                importance=TRUE, localImp = TRUE, ntree=private$n_trees)
                       self$training_data = data
                       
                       cat("# identifying important parameters ...\n")
                       private$identifyImportantParameters(configurations, experiments, force.dummy=TRUE)
                       
                       if (length(private$important_parameters)>1) {
                         # retrain the model
                         cat("# re-training model with important parameters ...\n")
                         self$model <- randomForest::randomForest(x=data$data[,private$important_parameters,drop=FALSE], 
                                                                  y=data$data$.PERFORMANCE., importance=TRUE, localImp = TRUE, 
                                                                  ntree=private$n_trees)                       
                       }
                       cat("# calculating interactions ...\n")
                       private$calculateInteractions()
                       
                       return (TRUE)
                     },
                     
                     # function isAllowedInteraction determines if root parameter rname
                     # can interact with parameter pname based on current dependencies
                     isAllowedInteraction = function(pname, rname, depends) {
                       # If there is no dependencies for the root parameter
                       # interaction is allowed
                       if (length(depends[[rname]]) < 0) 
                         return(TRUE)
                       
                       vars <- depends[[rname]]
                       
                       # check if parameters from which root rname depend will create
                       # a dependency cycle
                       for (var in vars) {
                         # The following lines detect cycles
                         if (var == pname)
                           return(FALSE)
                         
                         if (!self$isAllowedInteraction(pname, var, depends))
                           return(FALSE)
                       }
                       return (TRUE)
                     },
                     
                     # function updateSampleOrder that updates the sampling order of the parameters
                     # based on the hierarchy defined by the current parameter interaction 
                     generateSamplingOrder = function(extra.dependencies = NULL) {
                       cat("# generating sampling order ...\n")
                       cdependencies <- self$mergeDependencies(extra.dependencies)
                       
                       # calculate hierarchy
                       h <- sapply(private$parameters$names, private$treeLevel,
                                   varsTree = cdependencies)
                       #print(h)
                       self$sampling.order <- names(sort(h))
                     },
                     
                     # join the provided dependencies to current ones in the
                     # model
                     mergeDependencies = function(extra.dependencies) {
                       final <- private$parameters$depends
                       for (pname in names(extra.dependencies)){
                         final[[pname]] <- unique(c(final[[pname]],
                                                    extra.dependencies[[pname]]))
                       }
                       return(final)
                     }
                   ),
                   
                   private = list(
                     # parameters definition from irace
                     parameters = NULL,
                     
                     # seed obtained from the scenario of irace (for intermediate 
                     # temporary files purposes)
                     id_seed = 1234567,
                     
                     # number of trees to build in the random forest
                     n_trees = 50,
                     
                     # number of importan parameters to identify
                     n_imp_par = 5,
                     
                     # data frame obtained from the randomForestExplainer package
                     # that gives the importance of the parameters based on different 
                     # measures obtained from the forest
                     # * variable: parameter name
                     # * mean_mean_depth: mean min depth of the maximum trees in each tree
                     #    of the forest where variable is root
                     # * no_of_nodes: number of nodes that splits over variable
                     # * mse_increase: importance measeure obtained by the increment of mse based
                     #    on the use of variable as predictor
                     # * node_purity_increase: importance measeure obtained by the decrement of impurity
                     # based on the use of variable as predictor (makes more sense for classification)
                     # * no_of_trees: number of tree in which the variable is used as split
                     # * times_a_root: times the variable was used as a root of a tree of the forest
                     # * p_value: check documentation (i think p-value is related to only one measure)
                     importance_frame = NULL,
                     
                     # vector of important parameters
                     important_parameters = c(),
                     
                     # list of dependencies for each parameter 
                     # based on the original parameters$depends
                     # and adds over the new interactions detected
                     depends = c(), 
                     
                     ####################################################################
                     ################### data manipulation functions ####################
                     ####################################################################
                     
                     createData = function(configurations, experiments, remove.na.from=NULL, add.dummy=FALSE) {
                       # Drop not used columns (.ID., .PARENT.)
                       configurations <- configurations[, grep("^\\.", colnames(configurations), invert = TRUE),drop = FALSE]
                       rownames(configurations) <- NULL
                       
                       # Filter variables that are all NA (currently not supported)
                       configurations <- private$filterNACols(configurations)
                       if (ncol(configurations) < 2) {
                         cat ("# Only ", ncol(configurations)," parameters of the training ",
                              "data have NA values. Cannot train model.\n")
                         importance_frame <- NULL
                         important_parameters <- c()
                         return(NULL)
                       }
                       
                       # Filter NA rows for parameters in remove.na.from
                       if (!is.null(remove.na.from)) {
                         for (p in remove.na.from) {
                           index <- !is.na(configurations[,p])
                           configurations <- configurations[index, ,drop=FALSE]
                           experiments    <- experiments[,index, drop=FALSE]
                         }
                       }
                       if (nrow(configurations) < 2 || ncol(experiments) < 2) {
                         cat ("# Not enough data to train model after removal of configurations with ",
                              "parameters ",remove.na.from," set to NA\n")
                         return(NULL)
                       } 
                       
                       # Filter variables that have only one value
                       uvalues <- apply(configurations, 2, function(x) length(unique(x)))
                       non.equal <- names(uvalues)[uvalues != 1]
                       if (length(non.equal)<1) {
                         cat ("# Not enough data to train model after removal of ",
                              "parameters having only one value in the training set\n")
                         return(NULL)
                       } else {
                         cat ("# Only parameters ", non.equal, " have more than one value for training model\n")
                         configurations <- configurations[, non.equal, drop=FALSE]
                       }
                       
                       # Adding dummy parameter to filter non-important parameters and interactions
                       if (add.dummy) {
                         pnames <- colnames(configurations)
                         configurations <- cbind(sample(x = 1:10, size = nrow(configurations), replace = TRUE) ,configurations)
                         colnames(configurations) <- c("dummy", pnames)
                       }
                       
                       pnames     <- colnames(configurations)
                       inames     <- NULL
                       
                       lapply(1:nrow(experiments), private$dataNameBind, experiments=experiments, configurations=configurations)
                       data <- read.table(paste("rf-",private$id_seed,"-data.txt",sep=""), header=TRUE, sep=":", stringsAsFactors=TRUE)
                       data <- private$doImputeCols(data) 
                       
                       final.data <- list(data=data, pnames=c(pnames,"instance"))
                       return(final.data)
                     }, 
                     
                     createDataRank = function(configurations, experiments, remove.na.from=NULL, add.dummy=FALSE) {
                       # Drop not used columns (.ID., .PARENT.)
                       configurations <- configurations[, grep("^\\.", colnames(configurations), invert = TRUE),drop = FALSE]
                       rownames(configurations) <- NULL
                       
                       # Filter variables that are all NA (currently not supported)
                       configurations <- private$filterNACols(configurations)
                       if (ncol(configurations) < 2) {
                         cat ("# Only ", ncol(configurations)," parameters of the training ",
                              "data have NA values. Cannot train model.\n")
                         importance_frame <- NULL
                         important_parameters <- c()
                         return(NULL)
                       }
                       
                       # Filter NA rows for parameters in remove.na.from
                       if (!is.null(remove.na.from)) {
                         for (p in remove.na.from) {
                           index <- !is.na(configurations[,p])
                           configurations <- configurations[index, ,drop=FALSE]
                           experiments    <- experiments[,index, drop=FALSE]
                         }
                       }
                       if (nrow(configurations) < 2 || ncol(experiments) < 2) {
                         cat ("# Not enough data to train model after removal of configurations with ",
                              "parameters ",remove.na.from," set to NA\n")
                         return(NULL)
                       } 
                       
                       # Filter variables that have only one value
                       uvalues <- apply(configurations, 2, function(x) length(unique(x)))
                       non.equal <- names(uvalues)[uvalues != 1]
                       if (length(non.equal)<1) {
                         cat ("# Not enough data to train model after removal of ",
                              "parameters having only one value in the training set\n")
                         return(NULL)
                       } else {
                         cat ("# Only parameters ", non.equal, " have more than one value for training model\n")
                         configurations <- configurations[, non.equal, drop=FALSE]
                       }
                       
                       # Adding dummy parameter to filter non-important parameters and interactions
                       if (add.dummy) {
                         pnames <- colnames(configurations)
                         configurations <- cbind(sample(x = 1:10, size = nrow(configurations), replace = TRUE) ,configurations)
                         colnames(configurations) <- c("dummy", pnames)
                       }
                       
                       pnames     <- colnames(configurations)
                       inames     <- NULL
                       
                       # impute experiments by instance
                       for (i in 1:nrow(experiments)) {
                         experiments[i,is.na(experiments[i,])] = max(experiments[i,], na.rm=TRUE)
                       }
                       # rank configurations
                       for (i in 1:nrow(experiments)) {
                         experiments[i,] = rank(experiments[i,])
                       }     
                       
                       lapply(1:nrow(experiments), private$dataNameBind, experiments=experiments, configurations=configurations)
                       data <- read.table(paste("rf-",private$id_seed,"-data.txt",sep=""), header=TRUE, sep=":", stringsAsFactors=TRUE)
                       data <- private$doImputeCols(data) 
                       
                       final.data <- list(data=data, pnames=c(pnames,"instance"))
                       return(final.data)
                     }, 
                     
                     createDataInstance = function(configurations, experiments, remove.na.from=NULL, add.dummy=FALSE) {
                       # Drop not used columns (.ID., .PARENT.)
                       configurations <- configurations[, grep("^\\.", colnames(configurations), invert = TRUE),drop = FALSE]
                       rownames(configurations) <- NULL
                       
                       # Filter variables that are all NA (currently not supported)
                       configurations <- private$filterNACols(configurations)
                       if (ncol(configurations) < 2) {
                         cat ("# Only ", ncol(configurations)," parameters of the training ",
                              "data dont have NA values. Cannot train model.\n")
                         importance_frame <- NULL
                         important_parameters <- c()
                         return(NULL)
                       }
                       
                       # Filter NA rows for parameters in remove.na.from
                       if (!is.null(remove.na.from)) {
                         for (p in remove.na.from) {
                           index <- !is.na(configurations[,p])
                           configurations <- configurations[index, ,drop=FALSE]
                           experiments    <- experiments[,index, drop=FALSE]
                         }
                       }
                       if (nrow(configurations) < 2 || ncol(experiments) < 2) {
                         cat ("# Not enough data to train model after removal of configurations with ",
                              "parameters ",remove.na.from," set to NA\n")
                         return(NULL)
                       } 
                       
                       # Filter variables that have only one value
                       uvalues <- apply(configurations, 2, function(x) length(unique(x)))
                       non.equal <- names(uvalues)[uvalues != 1]
                       if (length(non.equal)<1) {
                         cat ("# Not enough data to train model after removal of ",
                              "parameters having only one value in the training set\n")
                         return(NULL)
                       } else {
                         cat ("# Only parameters ", non.equal, " have more than one value for training model\n")
                         configurations <- configurations[, non.equal, drop=FALSE]
                       }
                       
                       # Adding dummy parameter to filter non-important parameters and interactions
                       if (add.dummy) {
                         pnames <- colnames(configurations)
                         configurations <- cbind(sample(x = 1:10, size = nrow(configurations), replace = TRUE) ,configurations)
                         colnames(configurations) <- c("dummy", pnames)
                       }
                       
                       pnames     <- colnames(configurations)
                       inames     <- NULL
                       
                       # impute experiments by instance
                       for (i in 1:nrow(experiments)) {
                         experiments[i,is.na(experiments[i,])] = max(experiments[i,], na.rm=TRUE)
                       }
                       # rank configurations
                       for (i in 1:nrow(experiments)) {
                         experiments[i,] = rank(experiments[i,])
                       }     
                       
                       lapply(1:nrow(experiments), private$dataNameBind, experiments=experiments, configurations=configurations)
                       data <- read.table(paste("rf-",private$id_seed,"-data.txt",sep=""), header=TRUE, sep=":", stringsAsFactors=TRUE)
                       data <- private$doImputeCols(data) 
                       
                       final.data <- list(data=data, pnames=c(pnames,"instance"))
                       return(final.data)
                     },
                     
                     createDataInstance1 = function(configurations, experiments) {
                       # impute configurations by instance
                       for (i in 1:nrow(experiments)) {
                         experiments[i,is.na(experiments[i,])] = max(experiments[i,], na.rm=TRUE)
                       }
                       # rank configurations
                       for (i in 1:nrow(experiments)) {
                         experiments[i,] = rank(experiments[i,])
                       }                      
                       
                       configurations_surrogate = cbind(seq(1,nrow(configurations)), runif(n=nrow(configurations), min=1, max=100), 
                                                        runif(n=nrow(configurations), min=1, max=100), runif(n=nrow(configurations), min=1, max=100),
                                                        runif(n=nrow(configurations), min=1, max=100))
                       colnames(configurations_surrogate) = c("id", "dummy1","dummy2","dummy3", "dummy4")
                       pnames = c(colnames(configurations_surrogate))
                       
                       lapply(1:nrow(experiments), private$dataNameBind, experiments=experiments, configurations=configurations_surrogate)
                       data <- read.table(paste("rf-",private$id_seed,"-data.txt",sep=""), header=TRUE, sep=":", stringsAsFactors=TRUE)
                       #data <- private$doImputeCols(data) 
                       
                       pnames = c(pnames, "instance")
                       final.data <- list(data=data, pnames=pnames)
                       
                       return(final.data)
                     },
                     
                     dataNameBind = function (index, experiments, configurations, filter.na=TRUE, 
                                              file=paste("rf-",private$id_seed,"-data.txt",sep="")) {
                       
                       experiment <- experiments[index,]
                       
                       #remove infite experiments (rejected)
                       not.inf <- !is.infinite(experiment)
                       experiment <- experiment[not.inf]
                       if (length(experiment) < 1)
                         return
                       
                       not.na <- rep(TRUE, length(experiment))
                       if (filter.na) {
                         not.na <- !is.na(experiment)
                         experiment <- experiment[not.na]
                         if (length(experiment) < 1)
                           return
                       }
                       
                       all.configurations <- configurations[not.na,]
                       all.instances  <- matrix(paste("instance",index,sep=""), ncol=1, nrow=length(experiment))
                       
                       data           <- cbind(all.configurations, all.instances, experiment)
                       colnames(data) <- c(colnames(all.configurations), "instance", ".PERFORMANCE.")
                       
                       if (index==1)
                         write.table(data, append=FALSE, sep=":", row.names=FALSE, col.names=TRUE, quote=FALSE, file=file)
                       else
                         write.table(data, append=TRUE, sep=":", row.names=FALSE, col.names=FALSE, quote=FALSE, file=file)
                     },
                     
                     doImputeCols = function(data, pnames=NULL) {
                       performance.as.factor <- FALSE
                       
                       if(is.null(pnames))
                         pnames <- private$parameters$names  
                       
                       for (pname in pnames) {
                         if (!(pname %in% colnames(data))) next;
                         sel <- is.na(data[,pname])
                         if (sum(sel) >= 1){
                           if (private$parameters$types[pname] %in% c("r","i")) {
                             data[sel ,pname] <- private$parameters$domain[[pname]][2] * 2
                           } else if (private$parameters$types[pname] %in% c("c","o")) {
                             data[sel ,pname] <- "__miss__"
                           }
                         }
                         
                         if (private$parameters$types[pname] %in% c("c","o")) {
                           data[,pname] <- factor(data[,pname], ordered=FALSE)
                         }
                       }
                       
                       if (".PERFORMANCE." %in% colnames(data)){
                         data[,".PERFORMANCE."] <- as.numeric(data[,".PERFORMANCE."])
                       }
                       return(data)
                     },
                     
                     filterNACols = function(configurations) {
                       all.na <- which(colSums(is.na(configurations)) == nrow(configurations))
                       
                       if (length(all.na)>0) {
                         cat("Warning: Removing parameters with only NA values ", colnames(configurations)[all.na], "\n")
                         pnames <- colnames(configurations)[-all.na]
                         configurations <- configurations[,pnames, drop=FALSE]
                       }
                       return(configurations)
                     },
                     
                     filterFewInstancesConfigurations = function(configurations, experiments) {
                       # select configurations that have executed at least 5 instances
                       sel = apply(experiments, 2, function(x) {sum(!is.na(x)) }) > 4
                       experiments = experiments[,sel,drop=FALSE]
                       configurations = configurations[sel,,drop=FALSE]
                       # select instances with at least 2 configuration
                       sel = apply(experiments, 1, function(x) {sum(!is.na(x)) }) > 3
                       experiments = experiments[sel,]
                       ret = list(experiments=experiments, configurations=configurations)
                       return (ret)
                     },
                     
                     ####################################################################
                     ############# importance and interaction funcions ##################
                     ####################################################################
                     
                     # function identifyImportantParameters fills the variable important_parameters
                     # receives as in input the set of configurations and experiments used to build
                     # self$model
                     identifyImportantParameters = function(configurations, experiments, force.dummy=FALSE) {
                       # calculate parameter importance in the current model
                       private$importance_frame <- randomForestExplainer::measure_importance(self$model)
                       print(private$importance_frame)
                       
                       # get n_imp_par most important parameters
                       params <- randomForestExplainer::important_variables(private$importance_frame, k = private$n_imp_par, 
                                                                            measures = c("mean_min_depth", "no_of_trees"))
                       #cat("Initial important parameters: ", params,"\n")
                       
                       # check if there is conditional parameters between the important vars
                       pnames.to.remove <- c()
                       for (pname in params) {
                         if (pname!="dummy" && !private$isConditionalImportant(pname, configurations, experiments)) {
                           pnames.to.remove <- c(pnames.to.remove, pname)
                         } 
                       }
                       
                       # calculate interactions between parameters
                       private$important_parameters <- params[!(params %in% pnames.to.remove)]
                       cat("# important parameters after conditional removal: ", private$important_parameters,"\n")
                       
                       # If enforced, a dummy reference parameter is added to the list of important parameters
                       # this parameter can be used to discrimate real interactions.
                       # IMPORTANT: this parameter should be added to the dataset as a random uniformly sampled 
                       # predictor
                       if (force.dummy && !("dummy" %in% private$important_parameters)) {
                         cat("# adding a reference parameter ...\n")
                         private$important_parameters <- c(private$important_parameters, "dummy")
                       }
                       
                     },
                     
                     # function that returns a vector of important parameters
                     getImportantParameters = function() {
                       params <- params[params!= "dummy"]
                       return(params)
                     },
                     
                     aggregateInteractions = function() {
                       not.search = c()
                       self$full_interactions_frame = self$interactions_frame
                        for(i in 1:nrow(self$interactions_frame)) {
                          if (i %in% not.search)
                            next
                          interaction <- self$interactions_frame[i,,drop=FALSE]
                          inv <- paste(interaction[,"variable"],":",interaction[,"root_variable"], sep="")
                          w.i <- which(self$interactions_frame[,"interaction"] == inv)
                          if (length(w.i)>0){
                            self$interactions_frame[i,"mean_min_depth"] <- mean(self$interactions_frame[i,"mean_min_depth"], self$interactions_frame[w.i,"mean_min_depth"])
                            self$interactions_frame[i,"occurrences"] <- sum(self$interactions_frame[i,"occurrences"], self$interactions_frame[w.i,"occurrences"])
                            self$interactions_frame[i,"uncond_mean_min_depth"] <- mean(self$interactions_frame[i,"uncond_mean_min_depth"], self$interactions_frame[w.i,"uncond_mean_min_depth"])
                            not.search = c(not.search, w.i)
                          }
                        }
                       selected = 1:nrow(self$interactions_frame)
                       selected = selected[!(selected %in% not.search)]
                       self$interactions_frame = self$interactions_frame[selected,]
                       
                     },
                     
                     # Calculate interaction of parameters based on the combined minimal depth 
                     calculateInteractions = function(remove.inversed=TRUE) {
                       # calculate interactions
                       if (length(private$important_parameters)>0) {
                         cat ("# calculating interactions between selected parameters: ", private$important_parameters, "\n" )
                         self$interactions_frame <- randomForestExplainer::min_depth_interactions(self$model, private$important_parameters)
                       } else {
                         cat ("# calculating interactions between all parameters\n")
                         self$interactions_frame <- randomForestExplainer::min_depth_interactions(self$model)
                       }
                       # order them by number of occurrence of the interactions in the trees
                       self$interactions_frame <- self$interactions_frame[order(self$interactions_frame$occurrences, 
                                                                                decreasing=TRUE),]
                       # interaction aggregation (remove if aggregation param1->param2 and param2->param1 is not ok)
                       rownames(self$interactions_frame) <- NULL
                       self$full_interactions_frame = self$interactions_frame
                       private$aggregateInteractions()
                       #print(self$interactions_frame)
                       
                       # remove all interactions from the first occurrence of the 
                       # dummy reference variable
                       index <- NA
                       index.var <- self$interactions_frame$variable == "dummy"
                       index.root <- self$interactions_frame$root_variable == "dummy"
                       if (any(index.var) && any(index.root)) {
                         index <- min(min(which(self$interactions_frame$variable == "dummy"),
                                          which(self$interactions_frame$root_variable == "dummy")))
                       } else if (any(index.var)) {
                         index <- min(which(self$interactions_frame$variable == "dummy"))
                       } else if (any(index.root)) {
                         index <- min(which(self$interactions_frame$root_variable == "dummy"))
                       }
                       if (!is.na(index) && !is.infinite(index)) {
                         self$interactions_frame <- self$interactions_frame[-(index:nrow(self$interactions_frame)),]
                         rownames(self$interactions_frame) <- NULL
                       }
                       if (nrow(self$interactions_frame) < 1) return()
                       
                       # remove self interactions (localsearch:localsearch) there are 
                       # splits perfomed in different depths
                       sel <- !(self$interactions_frame$variable == self$interactions_frame$root_variable)
                       self$interactions_frame <- self$interactions_frame[sel,,drop=FALSE]
                       rownames(self$interactions_frame) <- NULL
                       #print(self$interactions_frame)
                       if (nrow(self$interactions_frame) < 1) return()
                       
                       # remove duplicated interactions (removed there should not be repeated interactions)
                       #sel <- match(unique(self$interactions_frame$interaction),self$interactions_frame$interaction)
                       #self$interactions_frame <- self$interactions_frame[sel,,drop=FALSE]
                       #rownames(self$interactions_frame) <- NULL
                       #print(self$interactions_frame)
                       
                       # remove interactions with a root parameter and its conditional activator
                       # as this is not possible. Ej. if parameter p1 is activated by parameter p2
                       # then parameter p2 cannot be root parameter of parameter p1 given that p1
                       # must be sampled in order to determine p2
                       sel <- c()
                       for (i in 1:nrow(self$interactions_frame)) {
                         d <- private$parameters$depends[[self$interactions_frame$root_variable[i]]]
                         if (!(self$interactions_frame$variable[i] %in% d)) {
                           sel <- c(sel, i)
                         }
                       }
                       self$interactions_frame <- self$interactions_frame[sel,,drop=FALSE]
                       rownames(self$interactions_frame) <- NULL
                       #print(self$interactions_frame)
                       if (nrow(self$interactions_frame) < 1) return()
                       
                       
                       # remove inversed interactions (keep the first). Ej. localsearch:ants -> ants:localsearch
                       if (remove.inversed) {
                         sel <- c()
                         for (i in 1:nrow(self$interactions_frame)) {
                           inverse_int <- paste(self$interactions_frame$variable[i],":", 
                                                self$interactions_frame$root_variable[i], sep="")
                           aux <- which(self$interactions_frame$interaction == inverse_int)
                           if (length(aux)> 0 ){
                             # select first
                             sel <- c(sel, min(aux,i))
                           } else {
                             sel <- c(sel, i)
                           }
                         }
                         self$interactions_frame <- self$interactions_frame[sort(unique(sel)),,drop=FALSE]
                         rownames(self$interactions_frame) <- NULL
                         #print(self$interactions_frame)
                         if (nrow(self$interactions_frame) < 1) return()
                       }
                       
                       # keep the most important interaction for each parameter
                       sel <- match(unique(self$interactions_frame$variable),self$interactions_frame$variable)
                       self$interactions_frame <- self$interactions_frame[sel,,drop=FALSE]
                       rownames(self$interactions_frame) <- NULL
                       #print(self$interactions_frame)
                       
                       # adding a sampling dependency is like adding a conditional parameter
                       # do not allow cyclical interactions (considering previous sampling dependencies)
                       private$depends <- private$parameters$depends
                       sel <- c()
                       for (i in 1:nrow(self$interactions_frame)) {
                         pname <- as.character(self$interactions_frame$variable[i])
                         rname <- as.character(self$interactions_frame$root_variable[i])
                         if (self$isAllowedInteraction(pname=pname, rname=rname, depends=private$depends))  {
                           sel <- c(sel, i)
                           private$depends[[pname]] <- unique(c(private$depends[[pname]], rname))
                         }
                       }
                       self$interactions_frame <- self$interactions_frame[sel,,drop=FALSE]
                       rownames(self$interactions_frame) <- NULL
                       print(self$interactions_frame)
                       
                     },
                     
                     # function isConditionalImportant checks if a conditional parameter is really
                     # important by filtering NA rows of this parameter.
                     # 
                     isConditionalImportant = function (pname, configurations, experiments) {
                       if (length(private$parameters$depends[[pname]]) > 0){
                         # remove NA rows from the data and check if the parameter is still is important
                         aux.data <- private$createData (configurations, experiments, remove.na.from=pname, add.dummy = TRUE)
                         if (is.null(aux.data)) {
                           # this should not happen, if it is the case, the parameter
                           # has only NA values
                           return(FALSE)
                         }
                         
                         cat("# Evaluating conditional parameter ", pname, " importance with parameters ",aux.data$pnames, "\n")
                         aux.model <- randomForest::randomForest(x=aux.data$data[,aux.data$pnames], y=aux.data$data$.PERFORMANCE., 
                                                                 importance=TRUE, localImp = TRUE, ntree=private$n_trees)
                         cat("# Calculating importance ... \n") 
                         aux.importance_frame <- randomForestExplainer::measure_importance(aux.model)
                         aux.params <- randomForestExplainer::important_variables(aux.importance_frame, k = private$n_imp_par, 
                                                                                  measures = c("mean_min_depth", "no_of_trees"))
                         if (pname %in% aux.params) {
                           return(TRUE)
                         } else {
                           return (FALSE)
                         }
                       } else {
                         return (TRUE)
                       }
                     },
                     
                     # function treeLevel that gets the hierarchical order of the parameters
                     # this function is a copy of the same function in th readParameters file
                     treeLevel = function(paramName, varsTree, rootParam = paramName) {
                       # The last parameter is used to record the root parameter of the
                       # recursive call in order to detect the presence of cycles.
                       vars <- varsTree[[paramName]]
                       if (length(vars) < 1) 
                         return (1) # This parameter does not have conditions
                       
                       # This parameter has some conditions
                       # Recursive call: level <- MAX( level(m) : m in children )
                       maxChildLevel <- 0
                       for (child in vars) {
                         # The following line detects cycles
                         if (child == rootParam)
                           irace.error("A cycle detected in subordinate sampling model parameters! ",
                                       "One parameter of this cycle is '", rootParam, "'")
                         
                         level <- private$treeLevel(child, varsTree, rootParam)
                         if (level > maxChildLevel)
                           maxChildLevel <- level
                       }
                       level <- maxChildLevel + 1
                       return (level)
                     }
                   )
)#End of RFModel