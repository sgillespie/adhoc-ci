#!/usr/bin/env bash

options=$(getopt -o h --long help -- "$@")

eval set -- "$options"

function print_help {
    cat <<EOF
Build and run tests when files change.

Usage: $0 [--] [TEST_TARGETS]
EOF

    exit 1
}

case $1 in
    -h|--help)
        print_help
        shift
        ;;
    --)
        shift
        ;;
esac

cabal --test-show-details=direct test all $@

watchman-make \
    --make "cabal --test-show-details=direct" \
    --pattern "*.cabal" "**/*.hs" --target "test all $@" \
    --settle 2
exit $?
