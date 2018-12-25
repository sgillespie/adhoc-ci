#/usr/bin/env bash
set -xe

if [[ -f ".adhoc-ci/build.sh" ]]; then
    bash .adhoc-ci/build.sh
else
    echo ".adhoc-ci/build.sh not found! Nothing to do"
fi
