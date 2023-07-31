#02.x_munge_sumstats.R

#This script munges the summary statistics before running ld-score regression

#load necessary packages
library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#specify settings
txt <- c("1.5y_diff1.txt","1.5y_tot1.txt","3y_diff2.txt","3y_tot2.txt","5y_diff3.txt","5y_tot3.txt")
hm3<- "//cluster/p/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/eur_w_ld_chr/w_hm3.noMHC.snplist"
trait.names <- c("diff1","tot1","diff2","tot2","diff3","tot3")

#munge sumstats
munge(files=txt,hm3=hm3,trait.names=trait.names)