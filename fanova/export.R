#!/usr/bin/env Rscript
#######################################################################
## export iRace data for processing with fANOVA
######################################################################
library(tibble)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
suppressPackageStartupMessages(library(optparse))
library(here)

source(paste(here(),"/model_scripts/DataHandling.R",sep=""))

options = list(
    make_option(c("-t","--type"), action="store", default="perf", help="Raw performance (perf), normalized performance (norm), performance quartile (quan), normalized ranking (rank), normalized ranking with imputation (irank), ranking quartile with imputation(qrank)")
)
opt=parse_args(OptionParser(option_list=options),positional_arguments=T)
if (length(opt$args)==0) {
    stop("Please give an irace data file name.\n")
}
datafile=opt$args[1]
load(datafile)

## (1) write all configurations
data = createData (configurations = iraceResults$allConfigurations,
                   experiments = iraceResults$experiments,
                   parameters = iraceResults$parameters,
                   add.dummy = TRUE,
                   add.instance=TRUE,
                   data.type = opt$options$type)

## (2) write fANOVA data
cat("Writing features and responses.\n")
write.table(data$data %>% select(-.PERFORMANCE.), file="features.csv", row.names = FALSE, sep=",", quote = FALSE)
write.table(data$data %>% select(.PERFORMANCE.),  file="response.csv", row.names = FALSE, col.names = FALSE, sep=",", quote = FALSE)
