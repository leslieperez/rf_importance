source("model_scripts/DataHandling.R")
source("model_scripts/RFModel.R")

# * Mtype: type of MODEL to build: 
#    - perf: performance (raw)
#    - norm: performance (normalized)
#    - quan: performance quartile
#    - rank: ranking (normalized)
#    - irank: ranking (including imputation)
#    - qrank: ranking quartile (including imputation)
train_and_save <- function (mtype, irace_file, save_file, n_trees=300){
  load(irace_file)
  
  parameters <- iraceResults$parameters
  scenario <- iraceResults$scenario
  configurations <- iraceResults$allConfigurations
  experiments <- iraceResults$experiments
  
  model = RFModel$new(n_trees, iraceResults$parameters, iraceResults$scenario)
  
  
  #    - perf: performance (raw)
  #    - norm: performance (normalized)
  #    - quan: performance quantile
  #    - rank: ranking (normalized)
  #    - irank: ranking (including imputation)
  #    - qrank: ranking quantile (including imputation)
  data = createData (configurations, experiments, add.dummy = TRUE , add.instance=TRUE, 
                     data.type = mtype , parameters = parameters)
  
  model$trainModel(data)
  
  cat("# Saving model..\n")
  save(model, experiments, configurations, parameters, scenario, file= save_file)
}

args = commandArgs(trailingOnly=TRUE)


mtype <- "perf"
irace_file <- "irace_data/irace-acotsp1000-4500-1.Rdata"
save_file <- "model_data/model-acotsp1000-4500-performance.Rdata"

if( length(args)>0) {
  mtype <- args[1]
  irace_file <- args[2]
  save_file <- args[3]
  n_trees <- as.numeric(args[4])
  train_and_save(mtype, irace_file, save_file, n_trees = n_trees)
}




