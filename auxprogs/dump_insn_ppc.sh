#!/bin/zsh

objdump -b binary -m powerpc -EB -D \
  =(perl -e 'print pack "N", hex $ARGV[0]' $1) | tail +7
