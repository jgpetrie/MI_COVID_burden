#!/bin/bash

#SBATCH --job-name=mcmc_65plus_assumed_mean
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --mem-per-cpu=32gb
#SBATCH --time=48:00:00
#SBATCH --partition=largemem
#SBATCH --get-user-env

echo Running job name $SLURM_JOB_NAME with ID $SLURM_JOB_ID on host $SLURM_SUBMIT_HOST;
echo Working directory is $SLURM_SUBMIT_DIR
echo Start time: `date`;


cd $SLURM_SUBMIT_DIR

age=65+
mean=7.550186
sd=105.3182
burn=10000
runs=50000
s1=.5
s2=.5
s3=.25
s4=.9
s5=3.9

Rscript "MCMC assumed mean.R" $age $mean $sd $burn $runs $s1 $s2 $s3 $s4 $s5
Rscript "MCMC summary assumed mean.R" $age $mean $sd $burn $runs

echo Finish time: `date`
