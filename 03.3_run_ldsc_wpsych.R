#03.3_run_ldsc_wpsych.R

#This script runs LD-score regression of differentiation and total problems, 
#and also the 11 psychiatric conditions. To be run on the cluster, using 
#the '03.3_run_ldsc_wpsych.sh' script in the 'shell scripts' folder.

library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(vroom, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#run ld-score regression
traits<- c("an.sumstats.gz","ocd.sumstats.gz","ts.sumstats.gz",
           "scz.sumstats.gz","bip.sumstats.gz","alc.sumstats.gz",
           "adhd.sumstats.gz","asd.sumstats.gz","ptsd.sumstats.gz",
           "mdd.sumstats.gz","anx.sumstats.gz",
           "diff1.sumstats.gz","tot1.sumstats.gz","diff2.sumstats.gz",
           "tot2.sumstats.gz","diff3.sumstats.gz","tot3.sumstats.gz")

sample.prev <- c(.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,NA,NA,NA,NA,NA,NA)

population.prev <- c(.009,.02,.007,.01,.01,.12,.087,.02,.068,.21,.31,NA,NA,NA,NA,NA,NA)

ld <- "/cluster/p/p471/cluster/data/genetic_data/for_deletion/qcd_genetic_data/regenie-master/eur_w_ld_chr/"

wld <- "/cluster/p/p471/cluster/data/genetic_data/for_deletion/qcd_genetic_data/regenie-master/eur_w_ld_chr/"

trait.names=c("an","ocd","ts","scz","bip","alc","adhd","asd","ptsd","mdd","anx",
              "diff1","tot1","diff2","tot2","diff3","tot3")

stand=TRUE

#run the ldsc function 
cov_wpsych <- ldsc(traits=traits,sample.prev=sample.prev,population.prev=population.prev, 
                   ld=ld,wld=wld,trait.names=trait.names,stand=stand)

save(cov_wpsych, file="cov_wpsych.RData")

