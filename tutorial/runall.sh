#!/bin/bash

TESTS="gcd combinational functional mux2 sequential parity memo filter tbl"
for t in $TESTS; do
  runone.sh $t > $t.res
  passed=`grep PASSED $t.res`
  failed=`grep FAILED $t.res`
  echo $t [$passed$failed]
  rm $t.res
done
