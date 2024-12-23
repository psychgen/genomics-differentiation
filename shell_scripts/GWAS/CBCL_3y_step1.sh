#!/bin/bash
#SBATCH --job-name=CBCL_3y_step1
#SBATCH --account=p471
#SBATCH --time=10:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=25G
#SBATCH --cpus-per-task=18



source /cluster/bin/jobsetup
module purge
module load singularity/3.7.3

set -o errexit

OUTM="/ess/p471/cluster/projects/differentiation_genomics/GWAS/output_files"

export SINGULARITYENV_LC_ALL=C

singularity exec -B /ess/p471/cluster /ess/p471/cluster/regenie_v3.1.1.gz.sif regenie \
--step 1 \
--bed /ess/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /ess/p471/cluster/projects/differentiation_genomics/scripts/GWAS/input_files/covar_gwas_3y.txt \
--phenoFile /ess/p471/cluster/projects/differentiation_genomics/scripts/GWAS/input_files/pheno_gwas_3y.txt \
--extract /ess/p471/cluster/projects/differentiation_genomics/scripts/GWAS/input_files/500k_N_genotyped_1+_info_0.97785.snps \
--bsize 1000 \
--strict \
--lowmem \
--lowmem-prefix tmp_rg_3y \
--out ${OUTM}/CBCL_3y
