#!/usr/bin/env bash
######################################################################
# run parameter analysis
######################################################################
base=$(git rev-parse --show-toplevel 2> /dev/null)
fanova="${base}/fanova"

types="perf norm quan rank irank qrank"
data="irace-acotsp1000-4500"
export="${fanova}/export.R"
process="${fanova}/process.py"

## preprocess
function preProcess() {
    mkdir input
    cd input
    for t in ${types}; do
	for i in 1 2 3 4 5; do
	    ${export} -t ${t} ${base}/irace_data/${data}-${i}.Rdata
	done
    done
}

# process_single: process a single log file
#
# name: base name; we expect to see name-perf.csv, name-norm.csv, etc.
# rep:  number of replications
function process_single() {
    name=$1
    rep=$2
    
    pf="_r"
    for t in ${types}; do
	for r in $(seq 1 ${rep}); do
	    if [ -e ${name}-${t}-features.csv ]; then
		echo "Processing ${name} type ${t} replication ${r}"
		${process} -s -r "${pf}${r}" ${name}-${t}
	    else
		echo "Skipping ${name} type ${t} replication ${r}"
	    fi
	done
    done
}

## process_all: process all five runs of the data file (for irace-acotsp1000-4500 only)
function process_all() {
    name=$1
    rep=$2
    for i in 1 2 3 4 5; do
	echo "Processing ${i} ${t}"
	process_single ${data}-${i} ${rep}
    done
}

#process_all ${data} 2
process_single "${data}-1" 5
