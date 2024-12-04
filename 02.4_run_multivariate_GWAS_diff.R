#02.4_run_multivariate_GWAS_diff.R

#This script runs multivariate GWAS of the differentiation intercept, from an 
#LGM that is equivalent to the phenotypic model, but specified based on a
#genetic variance/covariance matrix. To be run on the cluster using the script 
#'02.4_run_multivariate_GWAS_diff.sh' in the 'shell scripts/multivariate GWAS' folder.

library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

load(file="/ess/p471/data/durable/projects/differentiation_genomics/cluster/scripts/GWAS/input_files/ldsc.RData")
load(file="/ess/p471/data/durable/projects/differentiation_genomics/cluster/scripts/GWAS/input_files/sumstats.RData")

lgm.model <-
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3
           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           s1 ~~ s1
           i1 ~~ s1
           i2 ~~ i2
           s2 ~~ s2
           i2 ~~ s2
           i1 ~~ 0*i2
           s1 ~~ 0*s2
           i1 ~~ 0*s2
           s1 ~~ 0*i2
           
# snp effects

           i1 ~ SNP
           '

sub<-c("i1 ~ SNP")

GWAS_both_diff <-userGWAS(covstruc=ldsc,SNPs=sumstats,model=lgm.model,estimation="DWLS",
                          sub=sub,smooth_check=FALSE,cores=20,parallel=TRUE)

save(GWAS_both_diff, file="GWAS_both_diff.RData")
