#!/bin/bash
#SBATCH --job-name=diff_i_ldsc
#SBATCH --account=p471
#SBATCH --time=0:10:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10G


# To activate the conda environment, run: source /cluster/projects/p471/ldsc-activation
# To deactivate run: source deactivate.
# python3.gnu/3.7.3


source /cluster/bin/jobsetup
module purge
set -o errexit


# Filepath for inputs and outputs
INM="/ess/p471/cluster/projects/differentiation_genomics"
OUTM="/ess/p471/cluster/projects/differentiation_genomics/GWAS/output_files/genetic_correlations"


source /cluster/projects/p471/ldsc-activation

ldsc.py \
--rg intercept_diff.sumstats.gz,an.sumstats.gz,ocd.sumstats.gz,ts.sumstats.gz,scz.sumstats.gz,bip.sumstats.gz,alc.sumstats.gz,adhd.sumstats.gz,asd.sumstats.gz,ptsd.sumstats.gz,mdd.sumstats.gz,anx.sumstats.gz \
--ref-ld-chr /ess/p471/cluster/data/genetic_data/for_deletion/qcd_genetic_data/regenie-master/eur_w_ld_chr/ \
--w-ld-chr /ess/p471/cluster/data/genetic_data/for_deletion/qcd_genetic_data/regenie-master/eur_w_ld_chr/ \
--samp-prev nan,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5 \
--pop-prev nan,.009,.02,.007,.01,.01,.12,.087,.02,.068,.21,.31 \
--out pgc_diff_int_rgs