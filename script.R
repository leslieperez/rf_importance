#source("model_scripts/RFModel.R")
#source("model_scripts/RFQModel.R")
source("model_scripts/RFRModel.R")
#source("model_scripts/RFRQModel.R")

load("irace_data/irace-acotsp1000-4500-1.Rdata")
#load("irace_data/irace-acotsp1000-4500-low-budget.Rdata")
#load("irace_data/irace-acotsp2000-exp-num.Rdata")



parameters <- iraceResults$parameters
scenario <- iraceResults$scenario

#model = RFModel$new(300, iraceResults$parameters, iraceResults$scenario)
#model = RFQModel$new(300, iraceResults$parameters, iraceResults$scenario)
model = RFRModel$new(300, iraceResults$parameters, iraceResults$scenario)
#model = RFRQModel$new(300, iraceResults$parameters, iraceResults$scenario)

configurations = iraceResults$allConfigurations
experiments = iraceResults$experiments

model$trainModel(configurations, experiments, add.instance=TRUE)

save(model, experiments, configurations, parameters, scenario, file="model_data/modelr-acotsp1000-4500.Rdata")
#save(model, experiments, configurations, parameters, scenario, file="model_data/model-acotsp2000-qranking.Rdata")

