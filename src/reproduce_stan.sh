#!/bin/bash
FILES=diversity_data_Brachiopoda.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    trunc_test sample \
      adapt delta=0.999 \
      num_samples=5000 num_warmup=5000 thin=5\
      algorithm=hmc engine=nuts max_depth=15 stepsize=0.0001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=trunc_${i}_Brachiopoda.csv &
  done
  wait
done
FILES=diversity_data_Bivalvia.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    trunc_test sample \
      adapt delta=0.999 \
      num_samples=5000 num_warmup=5000 thin=5 \
      algorithm=hmc engine=nuts max_depth=15 stepsize=0.0001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=trunc_${i}_Bivalvia.csv &
  done
  wait
done
