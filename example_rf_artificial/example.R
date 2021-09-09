library("irace")
source("../model_scripts/DataHandling.R")
source("../model_scripts/RFModel.R")

data = list()

# read parameter definition
parameters = readParameters("parameters.txt")

# read the data set from a table
# - each parameter is a column
# - especial columns: instance and .PERFORMANCE.
# The instance column is not need, while .PERFORMANCE. is required
data$data = read.table("data.txt", header = TRUE)
data$pnames = colnames(data$data)[!(colnames(data$data) %in% ".PERFORMANCE.")]

#impute data
data$data = doImputeCols(data = data$data, parameters = parameters)

#build model
model = RFModel$new(n_trees=300, parameters=parameters)
#train 
model$trainModel(data=data)

#importance can be found in
model$importance_frame