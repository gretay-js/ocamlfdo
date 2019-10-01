#!/bin/bash
set -eu pipefail

echo "#HOW TO use ocamlfdo - simple example"
# run this from the top ocamlfdo directory
TEST=test2
EXE=$TEST.exe
PERF_DATA=$EXE.perf.data

echo ocamlfdo compile -extra-debug -- $TEST.ml -o $EXE
echo "\

# The following requires hardware support for performance monitoring.
# For accurate profile or if PMU is not available (e.g., when building on a VM),
# copy the executable to a benchmarking machine, run perf, and copy perf.data back.
"
echo perf record -e cycles:u -j any,u -o $PERF_DATA ./$EXE

echo ocamlfdo decode -binary $EXE -perf-profile $PERF_DATA

echo ocamlfdo linker-script -linker-script-hot $EXE.linker-script-hot -o $EXE.linker-script

echo ocamlfdo compile -fdo-profile $EXE.fdo-profile -- $TEST.ml -o $EXE.fdo -ccopt -Wl,--script=$EXE.linker-script -function-sections
