#!/usr/bin/env Rscript
#######################################################################
## export iRace data for processing with fANOVA
######################################################################
library(tibble)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
suppressPackageStartupMessages(library(optparse))

options = list()
opt=parse_args(OptionParser(option_list=options),positional_arguments=T)
if (length(opt$args)==0) {
    stop("Please give an irace data file name.\n")
}
datafile=opt$args[1]
load(datafile)

## (1) write all configurations

## (1.1) go over all results, collect info (parameter settings, instance, seed, value)
opt.instance=T  ## include instance in features?
opt.seed=F      ## include seed in features?

cat("Processing",datafile,"\n")
reslist=list()
ri=1
for(ii in 1:nrow(iraceResults$experiments)) {
    ins=iraceResults$state$.irace$instancesList[ii,]
    for(ci in 1:ncol(iraceResults$experiments)) {
        value=iraceResults$experiments[ii,ci]
        if (!is.na(value)) {
            cfg=iraceResults$allConfigurations %>% filter(.ID.==ci) %>% select(-.ID.,-.PARENT.)
            if (opt.instance) {
                cfg$instance=ins$instance
            }
            if (opt.seed) {
                cfg$seed=ins$seed
            }
            cfg$response=value
            reslist[[ri]] <- cfg
            ri = ri+1
        }
    }
}
result=bind_rows(reslist)

## (1.3) code strings
result$algorithm=as.numeric(as.factor(result$algorithm))
result$localsearch=as.numeric(as.factor(result$localsearch))
## "impute" NA with 10 times the domain's upper bound
result=result %>% replace_na(list(nnls=500,q0=10,dlb=10,rasrank=1000,elitistants=7500))
result$dlb=as.numeric(result$dlb)
                             
## (1.2) write fANOVA data
cat("Writing features and responses.\n")
write.table(result %>% select(-response), "features.csv",row.names=F,sep=",")
write.table(result %>% select(response), "response.csv" ,row.names=F,col.names=F,sep=",")
