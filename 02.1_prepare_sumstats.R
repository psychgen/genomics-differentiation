#02.1_prepare_sumstats.R

#The script prepares the summary statistics for multivariate GWAS in genomic SEM

#load necessary packages
library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#prepare sumstats for multivariate GWAS
files<-c("1.5y_diff1.txt","1.5y_tot1.txt","3y_diff2.txt","3y_tot2.txt","5y_diff3.txt","5y_tot3.txt")
ref <- "reference.1000G.maf.0.005.txt"
trait.names = c("diff1","tot1","diff2","tot2","diff3","tot3")
se.logit = c(T,T,T,T,T,T)
N = c(NA,NA,NA,NA,NA,NA)
OLS = c(T,T,T,T,T,T)

sumstats <-sumstats(files=files,ref=ref,trait.names=trait.names,se.logit=se.logit,OLS=OLS,N=N)

save(sumstats, file="sumstats.RData")