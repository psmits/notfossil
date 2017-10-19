#!/bin/bash
FILES=../data/data_dump/unit_data.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle sample num_samples=2000 num_warmup=2000 thin=2 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/hurdle_${i}.csv &
  done
  wait
done
