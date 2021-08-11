#!/usr/bin/env bash
######################################################################
# create
######################################################################
types="perf norm quan rank irank qrank"
base="irace-acotsp1000-4500"

for t in ${types}; do
    for i in 1 2 3 4 5; do
	../export.R -t ${t} ../../irace_data/${base}-${i}.Rdata
    done
done

for t in ${types}; do
    for i in 1 2 3 4 5; do
	echo "Processing ${i} ${t}"
	../process.py ${base}-${i}-${t}
    done
done
