#!/bin/bash

set -eu

if test "$#" != 2; then
    echo "usage: $0 <revo-data-dir> <output-dir>" >&2
    exit 1
fi

src_dir=$(dirname "$0")
tmp_dir=/tmp/prevo-db-dumpall

rm -rf "$tmp_dir"
mkdir -p "$tmp_dir"

"$src_dir/prevodb" -i "$1" -o "$tmp_dir"

mkdir -p "$2"

for x in "$tmp_dir/assets/articles/"*.bin; do
    txt="$2"/$(basename "$x" | sed 's/\.bin$/.txt/')
    "$src_dir/dump-article" "$x" > "$txt"
done

for x in "$tmp_dir/assets/indices/"*.bin; do
    txt="$2"/$(basename "$x" | sed 's/\.bin$/.txt/')
    "$src_dir/dump-index" "$x" > "$txt"
done
