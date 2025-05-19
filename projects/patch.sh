#!/usr/bin/env bash

input=$1
ldlibs=$(patchelf --print-needed ${input} | uniq | sort | sed '/\/nix\/store/d')

for lib in ${ldlibs}; do
	store_path=$(find /nix/store -maxdepth 5 -name ${lib} -not -path "*fhsenv*" -not -path "user-environment" -print -quit)
	patchelf "${input}" --replace-needed ${lib} ${store_path}
done
