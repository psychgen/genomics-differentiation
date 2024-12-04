#02.3_ldsc.R

#This script runs LDSC on the summary statistics for running multivariate GWAS.

#load necessary packages
library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(vroom, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#specify settings
trait.names <- c("diff1","tot1","diff2","tot2","diff3","tot3")
traits <- c("diff1.sumstats.gz","tot1.sumstats.gz","diff2.sumstats.gz",
            "tot2.sumstats.gz","diff3.sumstats.gz","tot3.sumstats.gz")
ld <- "//cluster/p/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/eur_w_ld_chr/"
wld <- "//cluster/p/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/eur_w_ld_chr/"
sample.prev <- c(NA,NA,NA,NA,NA,NA)
population.prev <- c(NA,NA,NA,NA,NA,NA)

ldsc <- ldsc(traits=traits, ld=ld, wld=wld, sample.prev=sample.prev, 
             trait.names=trait.names, population.prev=population.prev, stand=TRUE)

save(ldsc, file="ldsc.RData")