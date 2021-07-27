#!/usr/bin/env python3
import numpy as np
import ConfigSpace
import ConfigSpace.hyperparameters as hp
import pandas as pd
from fanova import fANOVA
import fanova.visualizer

## create a config space (manually for now)
cs = ConfigSpace.ConfigurationSpace()
cs.add_hyperparameter(hp.CategoricalHyperparameter('algorithm', choices=['0','1','2','3','4','5']))
cs.add_hyperparameter(hp.CategoricalHyperparameter('localsearch', choices=['0','1','2','3','4']))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('alpha', lower=0.0, upper=5.0))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('beta', lower=0.0, upper=10.0))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('rho', lower=0.0, upper=1.0))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('ants', lower=5, upper=100))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('nnls', lower=5, upper=500)) # imputed
cs.add_hyperparameter(hp.UniformFloatHyperparameter('q0', lower=0.0, upper=10.0))  # imputed
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('dlb', lower=0, upper=10))   # imputed
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('rasrank', lower=0, upper=1000)) # imputed
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('elitistants', lower=1, upper=7500)) # imputed
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('instance', lower=0, upper=100000)) # hack
pnames=['algorithm','localsearch','alpha','beta','rho','ants','nnls','q0','dlb','rasrank','elitistants','instance']

## execute fANOVA
X=pd.read_csv('features.csv')
Y = np.loadtxt('response.csv', delimiter=",")
f = fANOVA(X,Y,config_space=cs)

## print parameter importance
with open('importance.dat','w') as inf:
    print("parameter ind_imp tot_imp ind_std tot_std")
    for i in range(0,len(pnames)):
        imp=f.quantify_importance((pnames[i], ))[(pnames[i],)]
        print(pnames[i],imp['individual importance'],imp['total importance'],imp['individual std'],imp['total std'],file=inf)

## do the plots
vis = fanova.visualizer.Visualizer(f, cs, './plot')
vis.create_all_plots()
