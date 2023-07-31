#02_wave_specific_GWAS.R

#To facilitate the running of multivariate GWAS of differentiation and total
#problems modelled longitudinally using LGMs, we conducted wave-specific GWAS 
#of the difference and total scores in Regenie at the ages of 1.5, 3, and 5. 
#Scripts for conducting GWAS in Regenie are in the 'shell scripts/GWAS' folder.
#Below, we prepare the sumstats from wave-specific GWAS for multivariate GWAS.
#Summary statistics from these wave-specific GWAS are then prepared and munged,
#before running ldsc to enable inclusion in multivariate GWAS using genomic SEM.
#The script '02.1_prepare_sumstats' prepares the summary statistics, then
#munging is done in the script '02.2_munge_sumstats.R', and ld-score regression 
#is done in the script '02.3_ldsc.R'. Scripts for running these preparation 
#steps on the cluster are in the 'shell scripts/GWAS' folder

##process sumstats from wave-specific GWAS

#load necessary packages
library(data.table)
library(tidyverse)

#make list of output files to be processed
regenie <- list.files(path = "./GWAS/output_files/", pattern = ".regenie")

#create function to rename the output files, and
#exponentiate the p-values which are on the log scale
add_p <- function(file,name){
  name <- stringr::str_remove(file, "CBCL_")
  name <- stringr::str_remove(name, ".regenie")
  name <- stringr::str_remove(name, "step2_")
  
  result <- fread(paste0("./GWAS/output_files/",file), head=TRUE)
  result$P <- 10^(-result$LOG10P)
  write.table(result, paste0(name,".txt"), quote=F,row.names = F)
}

#create summary text files
purrr::map(regenie, add_p)

txt <- list.files(paste0("./GWAS/output_files/"),pattern = ".txt") 

##move files over to cluster