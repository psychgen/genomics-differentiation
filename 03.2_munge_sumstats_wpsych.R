#03.2_munge_sumstats_wpsych.R

#This script munges the summary statistics for the 11 psychiatric and
#neurodevelopmental conditions before running LD-score regression.

#load necessary packages
library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#specify settings
filepath <- "/ess/p471/data/durable/projects/differentiation_genomics/data/sstats/"

filenames <- c("an2019_withNeff.txt","ocd_withNeff.txt","ts_withNeff.txt",
               "scz2022_withNeff.txt","bip2021_withNeff.txt",
               "alcdep2018eur_withNeff.txt","adhd_withNeff.txt",
               "asd_withNeff.txt", "ptsd_withNeff.txt",
               "mdd2019_withNeff.txt","anx_withNeff.txt")

files <- paste0(filepath,filenames)
hm3<- "//cluster/p/p471/cluster/data/genetic_data/for_deletion/qcd_genetic_data/regenie-master/eur_w_ld_chr/w_hm3.noMHC.snplist"
trait.names = c("an","ocd","ts","scz","bip","alc","adhd","asd","ptsd","mdd","anx")

#munge sumstats
munge(files=files,hm3=hm3,trait.names=trait.names)