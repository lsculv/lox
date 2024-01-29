#!/bin/bash

set -e

argc=$#
argv=("$@")

if [[ $argc -ne 3 ]]; then
    echo 'Usage: bench.sh <old-clox> <new-clox> <benchmarks-dir>'
    exit 1
fi

old_clox="${argv[0]}"
new_clox="${argv[1]}"
benchmarks_dir="$(realpath "${argv[2]}")"

if [[ ! -f $old_clox ]]; then
    echo "Error: $old_clox: No such file or directory."
    exit 1
fi
if [[ ! -f $new_clox ]]; then
    echo "Error: $new_clox: No such file or directory."
    exit 1
fi
if [[ ! -x $old_clox ]]; then
    echo "Error: $old_clox: File must be executable."
    exit 1
fi
if [[ ! -x $new_clox ]]; then
    echo "Error: $new_clox: File must be executable."
    exit 1
fi

benchmarks="$(fd -e lox . "$benchmarks_dir")"
for benchmark in $benchmarks; do
    hyperfine --warmup=5 --shell=none "$old_clox $benchmark" "$new_clox $benchmark"
done
