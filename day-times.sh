#!/bin/bash

for i in $(seq -f "%02g" 1 25)
do
	time ./exec/day${i} day/${i}/input.txt
done
