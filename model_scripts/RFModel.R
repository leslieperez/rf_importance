require("randomForest")
require("randomForestExplainer")
# Vignettes de randomForestExplainer
# https://cran.r-project.org/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
# Thesis that explains the methods in randomForestExplainer
# https://rawgit.com/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer_Master_thesis.pdf
library("R6")

##########################################################################
###
### Performance based random forest
###
##########################################################################
##########################################################################
RFModel <- R6Class("RFModel",
                   public = list(
                     # model is an object obtained using the randomForest package
                     model = NULL,
                     training_data = NULL,
                     
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
  
                     initialize = function(n_trees, parameters, scenario) {
                       private$n_trees = n_trees
                       private$parameters = parameters
                       private$id_seed = scenario$seed
                       self$sampling.order = names(scenario$parameters$conditions)
                     },
                     
                     # function trainModel trains a random forest model, calculates important parameters and
                     # generates a data frame with their interactions
                     # * configurations: configurations data frame
                     # * experiments: experiment data frame
                     # * add.dummy: add a dummy predictor to the data set (used as reference variable)
                     # * add.instance: add instance as predictor in the data set
                     trainModel = function(data) {
                       self$training_data <- data

                       if (is.null(data)) {
                         cat ("# Warning: data provided by irace is does not allow to train the random ",
                              "forest model\n")
                         return (FALSE)
                       }
                       
                       # train the model
                       cat("# Training main random forest model ...\n")
                       self$model <- randomForest::randomForest(x=data$data[,data$pnames], y=data$data$.PERFORMANCE., 
                                                  importance=TRUE, localImp = TRUE, ntree=private$n_trees)
                       
                       cat("# Identifying important parameters ...\n")
                       private$identifyImportantParameters(configurations, experiments)
                       
                       # If enforced, a dummy reference parameter is added to the list of important parameters
                       # this parameter can be used to discriminate real interactions.
                       # IMPORTANT: this parameter should be added to the data set as a random uniformly sampled 
                       # predictor
                       if (("dummy" %in% colnames(self$training_data$data)) && !("dummy" %in% self$important_parameters)) {
                         cat("# Adding dummy reference parameter for interaction calculation ...\n")
                         self$important_parameters <- c(self$important_parameters, "dummy")
                       }
                       
                       if (length(self$important_parameters)>1) {
                         # retrain the model
                         cat("# Re-training model with important parameters:",self$important_parameters,"\n")
                         self$model <- randomForest::randomForest(x=data$data[,self$important_parameters,drop=FALSE], 
                                                                  y=data$data$.PERFORMANCE., importance=TRUE, localImp = TRUE, 
                                                                  ntree=private$n_trees)                       
                       }
                       cat("# Calculating interactions between important parameters...\n")
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
                       cat("# Generating sampling order ...\n")
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
                     },
                     
                     report = function(val) {
                       cat("################################################\n")
                       cat("## Parameter Importance\n")
                       cat("################################################\n")
                       cat("# Important variables (parameteres) identified are:\n")
                       cat(self$important_parameters[!(self$important_parameters %in% c("dummy","instance"))])
                       
                       if ("instance" %in% self$important_parameters) {
                         cat("# Note: instance detected as important... consider incrementing the firstTest option\n")
                         cat("#       since this indicates that the scenario might be heterogeneous\n")
                       }
                       
                       cat("################################################\n")
                       cat("## Parameter Interaction\n")
                       cat("################################################\n")      
                       if (nrow(self$interactions_frame) > 0) {
                         cat("# The interactions detected are: \n")
                         for (i in 1:nrow(self$interactions_frame)){
                           cat("#  - ", self$interactions_frame$interaction,"\n")
                         }
                         if ( "instance" %in% self$interactions_frame$root_variable || "instance" %in% self$interactions_frame$variable){
                           cat("# The instance was detected in an interaction!\n")
                           cat("# check these interacting variables as they might be useful to identify instance-dependent settings\n")
                         }
                       } else {
                         cat("# No interactions were detected\n")
                       }
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
                     
                     # list of dependencies for each parameter 
                     # based on the original parameters$depends
                     # and adds over the new interactions detected
                     depends = c(), 
                     
                     # measures to define importance
                     imeasures = c("mean_min_depth", "mse_increase", "times_a_root"),
                     
                     ####################################################################
                     ############# importance and interaction funcions ##################
                     ####################################################################
                     
                     # function identifyImportantParameters fills the variable important_parameters
                     # receives as in input the set of configurations and experiments used to build
                     # self$model
                     identifyImportantParameters = function(configurations, experiments, force.dummy=FALSE) {
                       # calculate parameter importance in the current model
                       self$importance_frame <- randomForestExplainer::measure_importance(self$model)
                       
                       # order by importance (mean_min_depth)
                       self$importance_frame <- self$importance_frame[order(self$importance_frame[,"mean_min_depth"]),]
                       print(self$importance_frame)
                       
                       # get n_imp_par most important parameters
                       params <- randomForestExplainer::important_variables(self$importance_frame, k = private$n_imp_par, 
                                                     measures = private$imeasures)
                       #cat("Initial important parameters: ", params,"\n")
                       
                       # check if there is conditional parameters between the important vars
                       pnames.to.remove <- c()
                       for (pname in params) {
                         if (pname!="dummy" && !private$isConditionalImportant(pname, configurations, experiments)) {
                           pnames.to.remove <- c(pnames.to.remove, pname)
                         } 
                       }
                       
                       # calculate interactions between parameters
                       self$important_parameters <- params[!(params %in% pnames.to.remove)]
                       cat("# Important parameters after conditional removal: ", self$important_parameters,"\n")
                     },
                     
                     # function that returns a vector of important parameters
                     getImportantParameters = function() {
                       params <- params[params!= "dummy"]
                       return(params)
                     },
                     
                     # aggregate inverse interactions. This is done to detect interactions
                     # without putting a lot of importance in the hierarchy of the detected
                     # interaction. (ej. param1:param2 and param2:param1 show some level of
                     # interaction between these two variables without having a hierarchical
                     # relationship in this interaction)
                     aggregateInteractions = function() {
                       cat("# Aggregating reversed interactions...\n")
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
                     # * remove.inversed: bool, remove inverse interactions (less relevant is removed)
                     # * aggregate.inversed: bool, aggregate (mean) importance of reversed interactions
                     calculateInteractions = function(remove.inversed=FALSE, aggregate.inversed=TRUE) {
                       # calculate interactions
                       if (length(self$important_parameters)>0) {
                         cat ("# Calculating interactions between selected parameters: ", self$important_parameters, "\n" )
                         self$interactions_frame <- randomForestExplainer::min_depth_interactions(self$model, self$important_parameters)
                       } else {
                         cat ("# Calculating interactions between all parameters\n")
                         self$interactions_frame <- randomForestExplainer::min_depth_interactions(self$model)
                       }
                       # order them by number of occurrence of the interactions in the trees
                       self$interactions_frame <- self$interactions_frame[order(self$interactions_frame$occurrences, 
                                                                                decreasing=TRUE),]
                       rownames(self$interactions_frame) <- NULL
                       #print(self$interactions_frame)
                       
                       # aggregate inverse interactions (ej. param1:param2 and param2:param1)
                       # the most 
                       if (aggregate.inversed) {
                         rownames(self$interactions_frame) <- NULL
                         self$full_interactions_frame = self$interactions_frame
                         private$aggregateInteractions()
                         #print(self$interactions_frame)
                       }
                       
                       # remove all interactions from the first ocurrence of the 
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
                         if (pname %in% c("instance","dummy") || rname %in% c("instance","dummy"))
                           next
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
                         aux.data <- reduceData(self$training_data, parameters=parameters, remove.na.from=pname)
                         #aux.data <- private$createData (configurations, experiments, remove.na.from=pname, add.dummy = TRUE)
                         if (is.null(aux.data)) {
                           # this should not happen, if it is the case, the parameter
                           # has only NA values
                           return(FALSE)
                         }
                         
                         cat("# Evaluating conditional parameter ", pname, " importance with parameters ",aux.data$pnames, "\n")
                         aux.model <- randomForest::randomForest(x=aux.data$data[,aux.data$pnames], y=aux.data$data$.PERFORMANCE., 
                                                   importance=TRUE, localImp = TRUE, ntree=private$n_trees)
                         cat("# Calculating importance of",pname," \n") 
                         aux.importance_frame <- randomForestExplainer::measure_importance(aux.model)
                         aux.params <- randomForestExplainer::important_variables(aux.importance_frame, k = private$n_imp_par, 
                                                           measures = private$imeasures)
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
                           stop("A cycle detected in subordinate sampling model parameters! ",
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

