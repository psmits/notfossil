#!/bin/bash
FILES=../data/data_dump/unit_data_Arthropoda.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_train sample \
      adapt delta=0.95 \
      num_samples=1000 num_warmup=1000 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      data file=$f \
      output file=../data/mcmc_out/hurdle_train_${i}_Arthropoda.csv &
  done
  wait
done
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_over_train sample \
      adapt delta=0.95 \
      num_samples=2000 num_warmup=2000 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      data file=$f \
      output file=../data/mcmc_out/hurdle_over_train_${i}_Arthropoda.csv &
  done
  wait
done
FILES=../data/data_dump/unit_data_Brachiopoda.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_train sample \
      adapt delta=0.95 \
      num_samples=1000 num_warmup=1000 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      data file=$f \
      output file=../data/mcmc_out/hurdle_train_${i}_Brachiopoda.csv &
  done
  wait
done
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_over_train sample \
      adapt delta=0.95 \
      num_samples=2000 num_warmup=2000 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      data file=$f \
      output file=../data/mcmc_out/hurdle_over_train_${i}_Brachiopoda.csv &
  done
  wait
done
