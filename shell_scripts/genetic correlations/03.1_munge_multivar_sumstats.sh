#!/bin/bash
#
# Job name (this is what the cluster will show as running - keep short):
#SBATCH --job-name=munge
#
#Project:
#SBATCH --account=p471
#
#Wall clock limit (change based on resources required):
#SBATCH --time=03:00:00 
#
#SBATCH --ntasks=1
#
#Output filename customization
#This specification is Jobname_User_JobID
#SBATCH --output=./output/%x_%u_%j.out
#
# Max memory usage (change based on resources required):
#SBATCH --mem-per-cpu=24G

## Set up job enviroment:

module purge
module load R/4.1.0-foss-2021a


Rscript ./03.1_munge_multivar_sumstats.R