#!/bin/bash
set -eu pipefail

echo "#HOW TO use ocamlfdo - simple example"
# run this from the top ocamlfdo directory
TEST=examples/simple/test2
BUILD_DIR=_build/default
EXE=$BUILD_DIR/$TEST.exe
PERF_DATA=$TEST.perf.data

echo cd $BUILD_DIR
echo ocamlfdo compile -- $TEST.ml -o $TEST
echo "\

# The following requires hardware support for performance monitoring.
# For accurate profile or if PMU is not available (e.g., when building on a VM),
# copy the executable to a benchmarking machine, run perf, and copy perf.data back.
"
echo perf record -e cycles:u -j any,u -o $PERF_DATA $EXE

echo ocamlfdo decode -binary $EXE -perf-profile $PERF_DATA

echo ocamlfdo linker-script -linker-script-hot $EXE.linker-script-hot -o $EXE.linker-script

echo ocamlfdo compile -fdo-profile $EXE.fdo-profile -- $TEST.ml -o $TEST.exe -ccopt '-Wl,--script=$EXE.linker-script'
