#!/bin/sh

set -x -eu pipefail

ocamlfdo compile -auto -fdo-profile $FDO_PROFILE -md5-unit -- "$@" -function-sections --ccopt -Wl,--script=$LINKER_SCRIPT

