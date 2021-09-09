#!/usr/bin/env python3
import argparse
import os
import numpy as np
import ConfigSpace
import ConfigSpace.hyperparameters as hp
import pandas as pd
from fanova import fANOVA
import fanova.visualizer
import random

## (1) read data
parser = argparse.ArgumentParser(description='Run fANOVA.')
parser.add_argument('Base',metavar="base",type=str,help='Base name of input files.')
parser.add_argument('--plot',action='store_true',help='Create plots')
parser.add_argument('-c','--configspace', type=str,default='acotsp',help='Configuratio space (acotsp, X4, X2, X2m).')
parser.add_argument('-i','--interaction',action='store_true',help='Determine interaction importance')
parser.add_argument('-s','--single',action='store_true',help='Determine single parameter importance')
parser.add_argument('-r','--replication',type=str,default='',help='Replication identifier')
parser.add_argument('-t','--ntrees',type=int,default=16,help='Number of trees')
parser.add_argument('-x','--ubac',type=float,default=1.0,help='(X4) Upper bound for A and C.')
parser.add_argument('-y','--ubbd',type=float,default=1.0,help='(X4) Upper bound for B and D.')
parser.add_argument('--tag',  type=str,default='missing',help='Tag for output.')
parser.add_argument('--seed',type=int,default=1,help='Number of trees')
args = parser.parse_args()

base=os.path.basename(args.Base)
random.seed(args.seed)
numpy.random_seed(args.seed)

X=pd.read_csv(args.Base+'-features.csv')
Y=np.loadtxt(args.Base+'-response.csv', delimiter=",")

## (2) create a config space (mostly manually for now)
cs = ConfigSpace.ConfigurationSpace()

if args.configspace == 'acotsp':
    #  recode categorical columns, since `low level library expects X argument to be float`
    categorical=['algorithm', 'dlb', 'instance']
    for cat in categorical:
        X[cat]=X[cat].astype('category').cat.codes

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
elif args.configspace == 'X4':
    ## artificial X4
    cs.add_hyperparameter(hp.UniformFloatHyperparameter('param_a', lower=0.0, upper=args.ubac))
    cs.add_hyperparameter(hp.UniformFloatHyperparameter('param_b', lower=0.0, upper=args.ubbd))
    cs.add_hyperparameter(hp.UniformFloatHyperparameter('param_c', lower=0.0, upper=args.ubac))
    cs.add_hyperparameter(hp.UniformFloatHyperparameter('param_d', lower=0.0, upper=args.ubbd))
elif args.configspace == 'X2m':
    ## artificial X2
    cs.add_hyperparameter(hp.UniformFloatHyperparameter('param_a', lower=0.0, upper=args.ubac))
    cs.add_hyperparameter(hp.UniformFloatHyperparameter('param_b', lower=0.0, upper=args.ubbd))

## (3) execute fANOVA
f = fANOVA(X,Y,config_space=cs,n_trees=args.ntrees)

## (4) output parameter importance
pnames=X.columns.tolist()

if args.single:
    #print("Individual importance.")
    #print(base,len(X),args.ntrees,args.tag,end=' ')
    with open(base+'-importance'+args.replication+'.dat','w') as inf:
        print("parameter ind_imp tot_imp ind_std tot_std",file=inf)
        for i in range(0,len(pnames)):
            imp=f.quantify_importance((pnames[i], ))[(pnames[i],)]
            #print(pnames[i],imp['individual importance'],imp['total importance'],imp['individual std'],imp['total std'],file=inf)
            print(base,replication,args.seed,len(X),args.ntrees,args.tag,pnames[i],imp['individual importance'])
            #print(imp['individual importance'],end=' ')
    #print()
    
## (5) output parameter interaction importance
## all pairs
if args.interaction:
    print("Interaction importance.")
    with open(base+'-interaction-importance.dat','w') as inf:
        print("parameter1 parameter2 ind_imp tot_imp ind_std tot_std",file=inf)
        for i in range(0,len(pnames)):
            for j in range(0,len(pnames)):
                if i==j:
                    continue
                print(pnames[i],pnames[j])
                imp=f.quantify_importance((pnames[i], pnames[j]))[(pnames[i], pnames[j])]
                print(pnames[i],pnames[j],imp['individual importance'],imp['total importance'],imp['individual std'],imp['total std'],file=inf)

## (6) n=10 most importante parameters via fANOVA: unused, since they just compute all and filter
#if args.interaction:
#    print("Interaction importance.")
#    mipm=f.get_most_important_pairwise_marginals()
#    with open(args.Base+'-interaction-importance'+args.replication+'.dat','w') as inf:
#        print("parameter1 parameter2 ind_imp tot_imp ind_std tot_std",file=inf)
#        for ia in mipm:
#                print(ia[0],ia[1])
#                print(ia[0],ia[1],mipm[ia],file=inf)

## (7) output the plots
if args.plot:
    print("Creating plots.")
    plotdir=base+'-plots'
    if not os.path.exists(plotdir):
        os.mkdir(plotdir)
        vis = fanova.visualizer.Visualizer(f, cs, plotdir)
        vis.create_all_plots()

## (8) dump a couple of predictions
with open(base+'-samples.dat','w') as saf:
    for i in range(0,1000):
        (a,b)=(random.random(),1*random.random()) ## fix
        v=f.the_forest.predict([a,b])
        print(a,b,v,file=saf)

## (9) dump the trees
#f.the_forest.save_latex_representation("./trees/test.tex")
