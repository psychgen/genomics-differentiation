#03.2_prepare_sumstats.R

#This script prepares the summary statistics for the 11 psychiatric conditions
#for genomic SEM. To be run on the cluster using the '03.2_prepare_sumstats.sh'
#script located in the 'shell scripts' folder

 library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
 library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")

#prepare sumstats for GWAS
 files<-c("./output/an2019_withNeff.txt","./output/ocd_withNeff.txt",  
          "./output/ts_withNeff.txt","./output/scz2022_withNeff.txt", 
          "./output/bip2021_withNeff.txt","./output/alcdep2018eur_withNeff.txt",
          "./output/adhd_withNeff.txt", "./output/asd_withNeff.txt",  
          "./output/ptsd_withNeff.txt","./output/mdd2019_withNeff.txt", 
          "./output/anx_withNeff.txt")
 
 ref <- "reference.1000G.maf.0.005.txt"
 
 trait.names = c("an","ocd","ts","scz","bip","alc","adhd","asd","ptsd","mdd","anx")
 
 se.logit = c(T,F,F,T,T,F,F,F,F,F,T)
#SE info: AN=beta,OCD=log(OR),TS=log(OR),SCZ=beta,BIP=beta,ALC=log(OR),ADHD=log(OR),ASD=log(OR),PTSD=log(OR),MDD=OR(F),ANX=Effect/beta(T)
 
 final_sumstats <-sumstats(files=files,ref=ref,trait.names=trait.names,se.logit=se.logit)

 save(final_sumstats, file="final_sumstats.RData")