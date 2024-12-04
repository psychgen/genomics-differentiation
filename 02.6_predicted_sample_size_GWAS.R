#02.6_predicted_sample_size_GWAS.R

#This script is used to check the output from the multivariate GWAS, and 
#calculate the predicted sample size for the intercept factors for tot and diff.

#load packages
library(data.table)
library(tidyverse)

#load output from GWAS of total problems
load(file="./GWAS/output_files/GWAS_both_tot.RData")

#select intercept tot
intercept_tot <- GWAS_both_tot[[1]] %>%
  select(-label) %>%
  rename(P=Pval_Estimate)

#look at fail messages
table(intercept_tot$fail)

#look at warning messages
table(intercept_tot$warning)

#check for genome-wide sig. hits
sig_itot <- intercept_tot %>%
  filter(P < 5e-8)

##calculate predicted sample size for the total problems intercept factor

#restrict to MAF of 40% and 10%
intercept_tot2<-subset(intercept_tot, intercept_tot$MAF <= .4 & intercept_tot$MAF >= .1)

#calculate expected sample size (N_hat)
N_hat_int_tot<-mean(1/((2*intercept_tot2$MAF*(1-intercept_tot2$MAF))*intercept_tot2$SE^2))

#add expected sample size to output
intercept_tot <- intercept_tot %>%
  mutate(N = rep(N_hat_int_tot,times = n()))

#write out
write.table(intercept_tot,file="intercept_tot_new.txt",
            sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)

#load output from GWAS of differentiation
load(file="./GWAS/output_files/GWAS_both_diff.RData")

#select intercept diff
intercept_diff <- GWAS_both_diff[[1]] %>%
  select(-label) %>%
  rename(P=Pval_Estimate)

#look at fail messages
table(intercept_diff$fail)

#look at warning messages
table(intercept_diff$warning)

#check for genome-wide sig. hits
sig_idiff <- intercept_diff %>%
  filter(P < 5e-8)

##calculate predicted sample size for the differentiation intercept factor

#restrict to MAF of 40% and 10%
intercept_diff2<-subset(intercept_diff, intercept_diff$MAF <= .4 & intercept_diff$MAF >= .1)

#calculate expected sample size (N_hat)
N_hat_intercept_diff<-mean(1/((2*intercept_diff2$MAF*(1-intercept_diff2$MAF))*intercept_diff2$SE^2))

#add expected sample size to output
intercept_diff <- intercept_diff %>%
  mutate(N = rep(N_hat_intercept_diff,times = n()))

#write out
write.table(intercept_diff,file="intercept_diff_new.txt",
            sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)
