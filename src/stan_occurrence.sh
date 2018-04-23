#!/bin/bash
# occurrence data
FILES=../data/data_dump/diversity_data_Brachiopoda_occurrence.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.99 \
      num_samples=10000 num_warmup=10000 thin=10\
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.1 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Brachiopoda_occurrence.csv &
  done
  wait
done
FILES=../data/data_dump/diversity_data_Trilobita_occurrence.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_multi sample \
      adapt delta=0.9999 \
      num_samples=10000 num_warmup=10000 thin=10 \
      algorithm=hmc engine=nuts max_depth=20 stepsize=0.001 \
      id=$i \
      init=0 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_Trilobita_occurrence.csv &
  done
  wait
done
#FILES=../data/data_dump/diversity_data_Bivalvia_occurrence.data.R
#for f in $FILES;
#do
#  for i in `seq 1 4`;
#  do
#    ../stan/trunc_multi sample \
#      adapt delta=0.99999999 \
#      num_samples=10000 num_warmup=10000 thin=10 \
#      algorithm=hmc engine=nuts max_depth=20 stepsize=0.0000001 \
#      id=$i \
#      init=0 \
#      data file=$f \
#      output file=../data/mcmc_out/trunc_${i}_Bivalvia_occurrence.csv &
#  done
#  wait
#done
#FILES=../data/data_dump/diversity_data_Gastropoda_occurrence.data.R
#for f in $FILES;
#do
#  for i in `seq 1 4`;
#  do
#    ../stan/trunc_multi sample \
#      adapt delta=0.9999 \
#      num_samples=10000 num_warmup=10000 thin=10 \
#      algorithm=hmc engine=nuts max_depth=20 stepsize=0.1 \
#      id=$i \
#      init=0 \
#      data file=$f \
#      output file=../data/mcmc_out/trunc_${i}_Gastropoda_occurrence.csv &
#  done
#  wait
#done
#FILES=../data/data_dump/diversity_data_Mollusca_occurrence.data.R
#for f in $FILES;
#do
#  for i in `seq 1 4`;
#  do
#    ../stan/trunc_multi sample \
#      adapt delta=0.9999 \
#      num_samples=10000 num_warmup=10000 thin=10 \
#      algorithm=hmc engine=nuts max_depth=20 stepsize=0.1 \
#      id=$i \
#      init=0 \
#      data file=$f \
#      output file=../data/mcmc_out/trunc_${i}_Mollusca_occurrence.csv &
#  done
#  wait
#done
#FILES=../data/data_dump/diversity_data_Anthozoa_occurrence.data.R
#for f in $FILES;
#do
#  for i in `seq 1 4`;
#  do
#    ../stan/trunc_multi sample \
#      adapt delta=0.99999999 \
#      num_samples=10000 num_warmup=10000 thin=10 \
#      algorithm=hmc engine=nuts max_depth=20 stepsize=0.0000001 \
#      id=$i \
#      init=0 \
#      data file=$f \
#      output file=../data/mcmc_out/trunc_${i}_Anthozoa_occurrence.csv &
#  done
#  wait
#done
#
