#!/usr/bin/env bash
######################################################################
# run parameter analysis
######################################################################
base=$(git rev-parse --show-toplevel 2> /dev/null)
fanova="${base}/fanova"
data="${fanova}/importance_reports/input"
types="perf norm quan rank irank qrank"
export="${fanova}/export.R"
process="${fanova}/process.py"

## preprocess
function preprocess() {
    local file=$1
    cd ${fanova}/importance_reports/input
    for t in ${types}; do
	${export} -t ${t} ${file}.Rdata
    done
}

# process_single: process a single log file
#
# name: base name; we expect to see name-perf.csv, name-norm.csv, etc.
# rep:  number of replications
function process() {
    name=$1
    rep=$2

    pf="_r"
    for t in ${types}; do
	for r in $(seq 1 ${rep}); do
	    if [ -e ${name}-${t}-features.csv ]; then
		echo "Processing $(basename ${name}) type ${t} replication ${r}"
		${process} -t 100 -s -r "${pf}${r}" ${name}-${t}
	    else
		echo "Skipping ${name} type ${t} replication ${r}"
	    fi
	done
    done
}

## preprocessing
#for i in {1..5}; do   preprocess ${base}/irace_data/irace-acotsp1000-4500-$i; done
#for i in {01..05}; do preprocess ${base}/experiments/irace_data/acotsp1000-4500-$i; done

## processing
#for i in {1..5}; do process ${data}/irace-acotsp1000-4500-$i; done
process ${data}/acotsp1000-4500-01 5
