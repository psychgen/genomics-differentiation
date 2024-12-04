#03.1_munge_multivar_sumstats.R

#This script munges the summary statistics from the multivariate GWAS
#of the differentiation and total problem intercepts. The munged 
#sumstats are then used to calculate genetic correlations in LDSC. 

#load necessary packages
library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#specify settings
txt <- c("intercept_diff_new.txt","intercept_tot_new.txt")
hm3<- "//cluster/p/p471/cluster/data/genetic_data/for_deletion/qcd_genetic_data/regenie-master/eur_w_ld_chr/w_hm3.noMHC.snplist"
trait.names <- c("intercept_diff","intercept_tot")

#munge sumstats
munge(files=txt,hm3=hm3,trait.names=trait.names)