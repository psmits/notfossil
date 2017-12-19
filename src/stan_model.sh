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
      random seed=100 \
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
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      random seed=100 \
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
      random seed=100 \
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
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      random seed=100 \
      data file=$f \
      output file=../data/mcmc_out/hurdle_over_train_${i}_Brachiopoda.csv &
  done
  wait
done
FILES=../data/data_dump/unit_data_Mollusca.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_train sample \
      adapt delta=0.95 \
      num_samples=1000 num_warmup=1000 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      random seed=100 \
      data file=$f \
      output file=../data/mcmc_out/hurdle_train_${i}_Mollusca.csv &
  done
  wait
done
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_over_train sample \
      adapt delta=0.95 \
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=15 \
      id=$i \
      random seed=100 \
      data file=$f \
      output file=../data/mcmc_out/hurdle_over_train_${i}_Mollusca.csv &
  done
  wait
done
# cross-validation stuff
for k in `seq 1 10`;
do
  for f in `seq 1 5`;
  do
    for i in `seq 1 4`;
    do
      ../stan/hurdle_train sample \
        adapt delta=0.95 \
        num_samples=1000 num_warmup=1000 \
        algorithm=hmc engine=nuts max_depth=15 \
        id=$i \
        random seed=100 \
        data file=../data/data_dump/unit_data_Arthropoda_fold${f}_rnds${k}.data.R \
        output file=../data/mcmc_out/hurdle_train_Arthropoda_fold${f}_rnds${k}_${i}.csv &
    done
    wait
    for i in `seq 1 4`;
    do
      ../stan/hurdle_over_train sample \
        adapt delta=0.95 \
        num_samples=1000 num_warmup=1000 \
        algorithm=hmc engine=nuts max_depth=15 \
        id=$i \
        random seed=100 \
        data file=../data/data_dump/unit_data_Arthropoda_fold${f}_rnds${k}.data.R \
        output file=../data/mcmc_out/hurdle_over_train_Arthropoda_fold${f}_rnds${k}_${i}.csv &
    done
    wait
    for i in `seq 1 4`;
    do
      ../stan/hurdle_train sample \
        adapt delta=0.95 \
        num_samples=1000 num_warmup=1000 \
        algorithm=hmc engine=nuts max_depth=15 \
        id=$i \
        random seed=100 \
        data file=../data/data_dump/unit_data_Brachiopoda_fold${f}_rnds${k}.data.R \
        output file=../data/mcmc_out/hurdle_train_Brachiopoda_fold${f}_rnds${k}_${i}.csv &
    done
    wait
    for i in `seq 1 4`;
    do
      ../stan/hurdle_over_train sample \
        adapt delta=0.95 \
        num_samples=1000 num_warmup=1000 \
        algorithm=hmc engine=nuts max_depth=15 \
        id=$i \
        random seed=100 \
        data file=../data/data_dump/unit_data_Brachiopoda_fold${f}_rnds${k}.data.R \
        output file=../data/mcmc_out/hurdle_over_train_Brachiopoda_fold${f}_rnds${k}_${i}.csv &
    done
    wait
    for i in `seq 1 4`;
    do
      ../stan/hurdle_train sample \
        adapt delta=0.95 \
        num_samples=1000 num_warmup=1000 \
        algorithm=hmc engine=nuts max_depth=15 \
        id=$i \
        random seed=100 \
        data file=../data/data_dump/unit_data_Mollusca_fold${f}_rnds${k}.data.R \
        output file=../data/mcmc_out/hurdle_train_Mollusca_fold${f}_rnds${k}_${i}.csv &
    done
    wait
    for i in `seq 1 4`;
    do
      ../stan/hurdle_over_train sample \
        adapt delta=0.95 \
        num_samples=1000 num_warmup=1000 \
        algorithm=hmc engine=nuts max_depth=15 \
        id=$i \
        random seed=100 \
        data file=../data/data_dump/unit_data_Mollusca_fold${f}_rnds${k}.data.R \
        output file=../data/mcmc_out/hurdle_over_train_Mollusca_fold${f}_rnds${k}_${i}.csv &
    done
    wait
  done
done
