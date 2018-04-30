#!/bin/bash
# diversity data
FILES=../data/data_dump/diversity_data_Brachiopoda_diversity.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.9999 \
      num_samples=10000 num_warmup=10000 thin=10\
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Brachiopoda_diversity.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Trilobita_diversity.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99999999 \
      num_samples=10000 num_warmup=10000 thin=10 \
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.00001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Trilobita_diversity.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Bivalvia_diversity.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99999999 \
      num_samples=10000 num_warmup=10000 thin=10 \
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.00001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Bivalvia_diversity.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Gastropoda_diversity.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.999999 \
      num_samples=10000 num_warmup=10000 thin=10 \
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.00001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Gastropoda_diversity.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Mollusca_diversity.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.999999 \
      num_samples=10000 num_warmup=10000 thin=10 \
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Mollusca_diversity.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Anthozoa_diversity.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99999999 \
      num_samples=10000 num_warmup=10000 thin=10 \
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.00001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Anthozoa_diversity.csv &
  done
  wait
done
