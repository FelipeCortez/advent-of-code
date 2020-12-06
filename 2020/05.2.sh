#!/usr/bin/env bash

gcc 05.2.c

./a.out < 05.in | \
    sort -g | \
    awk '{ if(($0 - n) != 1) { print $0 - 1 } n = $0}' | \
    tail -n1
