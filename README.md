# rf_importance

This repository contains 4 R classes that create random forest models based on irace performance data:
- model_scripts/RFModel.R: Creates a random forest model that predicts **raw performance**
- model_scripts/RFRModel.R: Creates a random forest model that predicts **ranking** of configurations
- model_scripts/RFQModel.R: Creates a random forest model that predicts the **quartile of performance**
- model_scripts/RFRQModel.R: Creates a random forest model that predicts the **quartile of the ranking**

To create the model all classes implement an initialization constructor:

`model = RFModel$new(n_trees, iraceResults$parameters, iraceResults$scenario)` 

Then the data to train the model must be provided:

`model$trainModel(iraceResults$allConfigurations, iraceResults$experiments, add.instance=TRUE)`

Some important variables of the class are:

- `model$model`: trained random forest model 
- `model$training_data`: data used to train the model
- `model$importance_frame`: matrix of importance measures
- `model$full_interactions_frame`: matrix of interaction importance measures

The file script.R provides an example of the use of these classes.
