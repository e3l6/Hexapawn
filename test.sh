#!/bin/bash
INFILES=test?-in.txt

for i in $INFILES
do
    ./hw2 < $i
done
