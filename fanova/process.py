#!/usr/bin/env python3
import argparse
import numpy as np
import ConfigSpace
import ConfigSpace.hyperparameters as hp
import pandas as pd
from fanova import fANOVA
import fanova.visualizer

## (1) read data
parser = argparse.ArgumentParser(description='Run fANOVA.')
parser.add_argument('Base',metavar="base",type=str,help='Base name of input files.')
parser.add_argument('-p','--plot',action='store_true',help='Create plots')
args = parser.parse_args()

X=pd.read_csv(args.Base+'-features.csv')
Y=np.loadtxt(args.Base+'-response.csv', delimiter=",")

## (2) recode categorical columns, since `low level library expects X argument to be float`
categorical=['algorithm', 'dlb', 'instance']
for cat in categorical:
    X[cat]=X[cat].astype('category').cat.codes

## (3) create a config space (mostly manually for now)
cs = ConfigSpace.ConfigurationSpace()
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('dummy', lower=1, upper=10))
#cs.add_hyperparameter(hp.CategoricalHyperparameter('algorithm', choices=list(set(X['algorithm'].tolist()))))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('algorithm', lower=min(X['algorithm']), upper=max(X['algorithm'])))
cs.add_hyperparameter(hp.CategoricalHyperparameter('localsearch', choices=['0','1','2','3']))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('alpha', lower=0.0, upper=5.0))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('beta', lower=0.0, upper=10.0))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('rho', lower=0.0, upper=1.0))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('ants', lower=5, upper=100))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('nnls', lower=5, upper=100))
cs.add_hyperparameter(hp.UniformFloatHyperparameter('q0', lower=0.0, upper=2.0))
#cs.add_hyperparameter(hp.CategoricalHyperparameter('dlb', choices=list(set(X['dlb'].tolist()))))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('dlb', lower=min(X['dlb']), upper=max(X['dlb'])))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('rasrank', lower=0, upper=200))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('elitistants', lower=1, upper=1500))
#cs.add_hyperparameter(hp.CategoricalHyperparameter('instance', choices=list(set(X['instance'].tolist()))))
cs.add_hyperparameter(hp.UniformIntegerHyperparameter('instance', lower=min(X['instance']), upper=max(X['instance'])))

## (4) execute fANOVA
f = fANOVA(X,Y,config_space=cs)

## (5) output parameter importance
pnames=X.columns.tolist()
with open(args.Base+'-importance.dat','w') as inf:
    print("parameter ind_imp tot_imp ind_std tot_std",file=inf)
    for i in range(0,len(pnames)):
        imp=f.quantify_importance((pnames[i], ))[(pnames[i],)]
        print(pnames[i],imp['individual importance'],imp['total importance'],imp['individual std'],imp['total std'],file=inf)

## (6) output the plots
if args.plot:
    print("Creating plots.")
    plotdir=args.Base+'-plots'
    if not os.path.exists(plotdir):
        os.mkdir(plotdir)
        vis = fanova.visualizer.Visualizer(f, cs, plotdir)
        vis.create_all_plots()
