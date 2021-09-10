suppressPackageStartupMessages(library(optparse))

source("../model_scripts/DataHandling.R")
source("../model_scripts/RFModel.R")

# * Mtype: type of MODEL to build: 
#    - perf: performance (raw)
#    - norm: performance (normalized)
#    - quan: performance quartile
#    - rank: ranking (normalized)
#    - irank: ranking (including imputation)
#    - qrank: ranking quartile (including imputation)
# *impute: type of imputation for missing numerical values
#    - out: Value outside the domain
#    - mean: Mean of the data
#    - mode: Mode of the data
#    - random: Random value
train_and_save <- function (mtype, irace_file, save_file, ntrees=300, impute="out"){
  load(irace_file)
  
  parameters <- iraceResults$parameters
  scenario <- iraceResults$scenario
  configurations <- iraceResults$allConfigurations
  experiments <- iraceResults$experiments
  
  model = RFModel$new(ntrees, iraceResults$parameters, iraceResults$scenario)
  
  data = createData (configurations, experiments, add.dummy = TRUE , add.instance=TRUE, 
                     data.type = mtype , parameters = parameters, imputation=impute)
  
  model$trainModel(data)
  
  cat("# Saving model..\n")
  save(model, experiments, configurations, parameters, scenario, file= save_file)
}


options = list(
  make_option(c("-m","--model"), action="store", default="perf", 
              help="Raw performance (perf), normalized performance (norm), performance quartile (quan), normalized ranking (rank), normalized ranking with imputation (irank), ranking quartile with imputation (qrank)"),
  make_option(c("-r","--rep"), type="integer", default=5, 
              help="Number of repetitions"),
  make_option(c("-t","--trees"), type="integer", default=300, 
              help="Number of trees"),
  make_option(c("-i","--impute"), action="store", default="out", 
              help="Out of bound (out), mean (mean), mode, (mode), random (random)")
)
opt=parse_args(OptionParser(option_list=options),positional_arguments=T)
if (length(opt$args)==0) {
  stop("Please give an irace data file name and path to save the model.\n")
}



irace_file <- opt$args[1]
save_file <- opt$args[2]
mtype <- opt$options$model
repetitions <- opt$options$rep
ntrees <- opt$options$trees
impute <- opt$options$impute


if( length(args)>0) {
  for (i in 1:repetitions) {
    csave_file <- paste0(save_file, "-r0", i, ".Rdata")
    cat(paste0("Modeling: ", irace_file, ", model: ", mtype, ", impute: ", impute, ", ntrees: ", ntrees, ", save file: ", csave_file, "\n"))
    train_and_save(mtype, irace_file, csave_file, ntrees, impute)
  }
}




