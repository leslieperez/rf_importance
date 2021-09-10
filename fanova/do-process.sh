#!/usr/bin/env bash
######################################################################
# do-process: systematic study for artificial scenarios
######################################################################
repos=$(cd $(git rev-parse --show-cdup) .; pwd)
basic=${repos}/../parameter_analysis/basic_definitions/src

function generate() {
    local samples=$1
    local impute=$2

    (
	cd ${basic}
	./datagen.py -n ${samples} -i ${impute}
    )
}

nsamples="250 500 1000 2000 4000 8000"
vimpute="0.5 1.0 2.0 4.0 8.0"
ntrees="5 10 100 300"

function evaluate() {
    local tag="$1"
    local vimpute=$2
    if [ "${tag}" == "RD" ]; then
	for t in ${ntrees}; do
	    ./process.py -s -t ${t} --tag "${tag}" artificialX4
	done
    elif [ "${tag}" == "SD" ]; then
	for t in ${ntrees}; do
	    ./process.py -s -t ${t} --tag "${tag}" -y ${vimpute} artificialX4
	done
    else # LD
	for t in ${ntrees}; do
	    ./process.py -s -t ${t} --tag "${tag}" -x ${vimpute} -y ${vimpute} artificialX4
	done
    fi
}

for i in ${nsamples}; do
    generate ${i} random
    evaluate "RD NA" 1.0
    for j in ${vimpute}; do
	if [ "${j}" == "0.5" ]; then
	   ub="1.0"
	else
	    ub=${j}
	fi
	generate ${i} ${j}
	evaluate "SD ${j}" ${ub}
	evaluate "LD ${j}" ${ub}
    done
done
