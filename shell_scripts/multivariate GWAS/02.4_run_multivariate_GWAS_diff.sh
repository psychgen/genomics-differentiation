#!/bin/bash
#
# Job name (this is what the cluster will show as running - keep short):
#SBATCH --job-name=GWAS_diff
#
#Project:
#SBATCH --account=p471
#
#Wall clock limit (change based on resources required):
#SBATCH --time=2-12:00:00
#
#SBATCH --ntasks=1
#
#Output filename customization
#This specification is Jobname_User_JobID
#SBATCH --output=./output/%x_%u_%j.out
#
# Max memory usage (change based on resources required):
#SBATCH --mem-per-cpu=5G
#
# Number of processors
#SBATCH --cpus-per-task=20

## Set up job enviroment:

module purge
module load R/4.1.0-foss-2021a

export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1

Rscript ./02.4_run_multivariate_GWAS_diff.R