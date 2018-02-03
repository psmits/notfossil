#!/bin/bash
FILES=../data/data_dump/diversity_data_Brachiopoda.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99 \
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=10 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Brachiopoda.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Trilobita.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99 \
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=10 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Trilobita.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Bivalvia.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99 \
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=10 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Bivalvia.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Gastropoda.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99 \
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=10 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Gastropoda.csv &
  done
  wait
done
