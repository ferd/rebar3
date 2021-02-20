#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: vendor_verl.sh PATH_TO_VERL"
  exit 1
fi

REBAR3_TOP=$(pwd)
export REBAR3_TOP
pushd "$1"
cp src/verl.erl $REBAR3_TOP/src/vendored/r3_verl.erl
cp src/verl_parser.erl $REBAR3_TOP/src/vendored/r3_verl_parser.erl
cp LICENSE $REBAR3_TOP/src/vendored/VERL_LICENSE
popd

sed -i s/verl_parser:/r3_verl_parser:/g $REBAR3_TOP/src/vendored/r3_verl.erl
sed -i s/verl\)/r3_verl\)/g $REBAR3_TOP/src/vendored/r3_verl.erl
sed -i s/verl_parser/r3_verl_parser/g $REBAR3_TOP/src/vendored/r3_verl_parser.erl
sed -i s/verl:/r3_verl:/g $REBAR3_TOP/src/vendored/r3_verl_parser.erl
