#!/bin/bash

try() {
  expected="$1"
  input="$2"

  ./mlis $input
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expected, but got $actual"
    exit 1
  fi
}

try 0 0
try 1 1

echo ok
