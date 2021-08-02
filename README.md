# rf_importance

This repository contains an R class that create random forest models based on irace configuration  data:
- `model_scripts/RFModel.R`: Creates a random forest model and calculates importance and interactions
- `model_scripts/DataHandling.R`: Creates a training data set out of an irace data  

** Note: You must source both R files in order to create the model.**

To create the model all classes implement an initialization constructor:

```
model = RFModel$new(n_trees, iraceResults$parameters, iraceResults$scenario)
``` 

To create the data to train the model:

```
load("irace.Rdata")
data = createData (configurations = iraceResults$allConfigurations, 
                   experiments = iraceResults$experiments, 
                   parameters = iraceResults$parameters, 
                   add.dummy = TRUE, 
                   add.instance=TRUE, 
                   data.type = "perf")
```

The data.type argument defined the variable to be predicted:

- perf: performance (raw)
- norm: performance (normalized)
- quan: performance quantile
- rank: ranking (normalized)
- irank: ranking (including imputation)
- qrank: ranking quantile (including imputation)


Finally, to train :

```
model$trainModel(data)
```

## Simpler interface

The file `script.R` provides a simpler interface to use these classes. It can be used from the terminal or the R console and the only requirement is that the current directory for executing it should be the root of this repository.

From the terminal:

```
Rscript script.R model_type path/to/irace-data.Rdata path/to/save-model.Rdata
```

From the R console:

```
source("script.R")
train_and_save(mtype="perf", irace_file="path/to/irace-data.Rdata", save_file="path/to/save-model.Rdata")
```

## Export data to csv
Once the model is built or when you create a training data set you can export the training data to a csv file:

```
load("path/to/saved-model.Rdata")
write.table(model$training_data$data, sep = ":", file="path/to/data.csv", row.names = FALSE, quote = FALSE)
```


## Other details

Some important variables of the `RFModel` class are:

- `model$model`: trained random forest model 
- `model$training_data`: data used to train the model
- `model$importance_frame`: matrix of importance measures
- `model$full_interactions_frame`: matrix of interaction importance measures

