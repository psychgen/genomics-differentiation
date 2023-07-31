#03.1_calculate_Neff_sumstats.R

## This script calculates the sample size (N) as the sum of 
## effective sample sizes for each GWAS. 

#load in required packages
 library(data.table)
 library(dplyr)
 library(tidyr)
 library(stringr)
 library(genotools)

#############
#### MDD ####
#############
 
#read in the data file from Howard et al. (2019)
 MDD<-fread("N:/durable/projects/differentiation_genomics/data/sstats/PGC_UKB_depression_genome-wide.txt",data.table=FALSE)
 
#convert allele frequency column to minor allele frequency for effective sample size calculation below
 MDD$MAF<-ifelse(MDD$Freq > .5, 1-MDD$Freq, MDD$Freq)
 
#remove Freq column now that we have created MAF column
 MDD$Freq<-NULL
 
#calculate SNP-specific sum of effective sample size
 MDD$Neff<-4/((2*MDD$MAF*(1-MDD$MAF))*MDD$StdErrLogOR^2)
 
#calculate total effective N to cap backed out Neff
#these are the case control numbers from table 1 of Howard et al. (2019)
 Ncases<-127552+43204
 Ncontrols<-233763+95680
 v<-Ncases/(Ncases+Ncontrols)
 TotalNeff<-4*v*(1-v)*(Ncases+Ncontrols)
 
#cap at 1.1 of total effective N
 MDD$Neff<-ifelse(MDD$Neff > 1.1*TotalNeff, 1.1*TotalNeff, MDD$Neff)
 
#lower limit of 0.5 of total effective N
 MDD$Neff<-ifelse(MDD$Neff < 0.5*TotalNeff, 0.5*TotalNeff, MDD$Neff)
 
#output the updated MDD file
 write.table(MDD, file = "./data/sstats/mdd2019_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)

 rm(MDD)
 
#############
#### SCZ ####
#############

#read in schizophrenia summary data to edit the Neff column
 SCZ<-fread("N:/durable/projects/differentiation_genomics/data/sstats/PGC3_SCZ_wave3.primary.autosome.public.v3.vcf.tsv",data.table=FALSE)

#multiply Neff column by two (unlikely to be the correct type of Neff)
 SCZ$Neff <- SCZ$NEFFDIV2*2
 
#check if max value exceeds manually calculated approximate Neff
 max(SCZ$Neff,na.rm=TRUE)#170114.7 this is higher than the below calculation
 
#to check the above against manually calculated approximate (*) Neff,
#read in information about sample size per cohort from the Sample description table
#from Trubetskoy et al. (2022). *Note: this list excludes non-European samples,
#which were included in the GWAS meta-analysis:
#African American (6152 cases, 3918 controls), Latino (1234 cases, 3090 controls),
#and East Asian (kor1 - cno1 below)
 
 SCZ_N<-fread("./data/cohort_info/SCZcohorts.txt",header=TRUE)
 
#add non-eur to calculate overall number of cases and controls in primary GWAS 
 SCZ_N <- SCZ_N %>%
    add_row(dataset = "latino", cases = 1234, controls = 3090) %>%
    add_row(dataset = "african-american", cases = 6152, controls = 3918) %>%
    add_row(dataset = "kor1", cases = 688, controls = 492) %>%
    add_row(dataset = "jpn1", cases = 547, controls = 540) %>%
    add_row(dataset = "umc1", cases = 2328, controls = 2380) %>%
    add_row(dataset = "imh1", cases = 898, controls = 996) %>%
    add_row(dataset = "imh2", cases = 821, controls = 956) %>%
    add_row(dataset = "xju1", cases = 1902, controls = 1009) %>%
    add_row(dataset = "tai1", cases = 1123, controls = 2243) %>%
    add_row(dataset = "tai2", cases = 593, controls = 1190) %>%
    add_row(dataset = "uwa1", cases = 996, controls = 1047) %>%
    add_row(dataset = "hku1", cases = 476, controls = 2018) %>%
    add_row(dataset = "bix1", cases = 1047, controls = 2301) %>%
    add_row(dataset = "bix2", cases = 1021, controls = 1001) %>%
    add_row(dataset = "bix3", cases = 492, controls = 679) %>%
    add_row(dataset = "cno1", cases = 1332, controls = 2036)
 
#calculate sample prevalence for each cohort
 SCZ_N$v<-SCZ_N$cases/(SCZ_N$cases+SCZ_N$controls)
 
#calculate cohort specific effective sample size
 SCZ_N$Neff<-4*SCZ_N$v*(1-SCZ_N$v)*(SCZ_N$cases+SCZ_N$controls)
 
#calculate approximate sum of effective sample size: 161511.8
 sum(SCZ_N$Neff)
 
##back out approximate Neff using reference panel MAF instead
##since the needed information seems to be unavailable
 
 rm(SCZ,SCZ_N)
 
#############
#### BIP ####
#############
 
 BIP<-fread("N:/durable/projects/differentiation_genomics/data/sstats/BIP.QC.wave3noNOR",data.table=FALSE)
 
 max(BIP$N)#higher than sum of Neff below, not suitable for genomic SEM, delete
 
 BIP$N<-NULL
 
#read in information about sample size per cohort from Table S1 of Mullins et al. 2021
 BIP_N<-fread("./data/cohort_info/BIPcohorts.txt",header=TRUE)
 
#remove Norwegian cohorts that were removed from the BIP sumstats to avoid overlap
 BIP_N <- BIP_N %>%
    filter(`Sample abbreviation`!= "HUNT") %>%
    filter(`Sample abbreviation`!= "norgs") %>%
    filter(`Sample abbreviation`!= "noroe") %>%
    filter(`Sample abbreviation`!= "top7") %>%
    filter(`Sample abbreviation`!= "top8")
 
#calculate sample prevalence for each cohort
 BIP_N$v<-BIP_N$cases/(BIP_N$cases+BIP_N$controls)
 
#calculate cohort specific effective sample size
 BIP_N$Neff<-4*BIP_N$v*(1-BIP_N$v)*(BIP_N$cases+BIP_N$controls)
 
#calculate sum of effective sample size: 95792.56
 sum(BIP_N$Neff)
 
#add new Neff column to BIP sumstats
 BIP <- BIP %>%
    mutate(Neff = rep(95792.56,times=9289780))
 
 write.table(BIP, file = "./data/sstats/bip2021_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(BIP,BIP_N)
 
#############
#### AN #####
#############
 
#read in anorexia nervosa summary data to edit the Neff column
 AN<-fread("N:/durable/projects/differentiation_genomics/data/sstats/pgcAN2.2019-07.vcf.tsv",data.table=FALSE)
 
#multiply Neff column by two (seems to be the correct type of Neff)
 AN$Neff <- AN$NEFFDIV2*2
 
#check max value and compare to manually calculated Neff below
 max(AN$Neff)#46321.9, which is identical to the below - keep this Neff
 
#remove NEFFDIV2 column now that we have created Neff column
 AN$NEFFDIV2<-NULL
 
#read in information about sample size per cohort from Table S1of Watson et al. 2019
 AN_N<-fread("./data/cohort_info/ANcohorts.txt",header=TRUE)
 
#calculate sample prevalence for each cohort
 AN_N$v<-AN_N$cases/(AN_N$cases+AN_N$controls)
 
#calculate cohort specific effective sample size
 AN_N$Neff<-4*AN_N$v*(1-AN_N$v)*(AN_N$cases+AN_N$controls)
 
#calculate sum of effective sample size: 46321.9
 sum(AN_N$Neff)
 
 write.table(AN, file = "./data/sstats/an2019_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(AN,AN_N)
 
#############
#### ALC ####
#############
 
#read in alcohol data to make rsID
 ALCH<-fread("N:/durable/projects/differentiation_genomics/data/sstats/pgc_alcdep.eur_unrel_genotyped.aug2018_release.txt",data.table=FALSE)
 
#reformat SNP column to only include rsID
 ALCH$SNP<-sapply(strsplit(ALCH$SNP, ":"), '[', 1)
 
#output datafile
 write.table(ALCH, file = "./data/sstats/alcdep2018eur_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(ALCH)
 
##############
#### PTSD ####
##############
 
#read in PTSD sumstats
 PTSD<-fread("N:/durable/projects/differentiation_genomics/data/sstats/SORTED_PTSD_EA9_ALL_study_specific_PCs1.txt",data.table=FALSE)
 
#read in information about sample size per cohort
 PTSD_N<-read.csv("./data/cohort_info/PTSDcohorts.csv",header=TRUE)
 
#calculate sample prevalence for each cohort
 PTSD_N$v<-PTSD_N$cases/(PTSD_N$cases+PTSD_N$controls)
 
#calculate cohort specific effective sample size
 PTSD_N$Neff<-4*PTSD_N$v*(1-PTSD_N$v)*(PTSD_N$cases+PTSD_N$controls)
 
#calculate sum of effective sample size: 5831.346
 sum(PTSD_N$Neff)
 
#add Neff column to sumstats
 PTSD <- PTSD %>%
    mutate(Neff = rep(5831.346,times=13206891))
 
 write.table(PTSD, file = "./data/sstats/ptsd_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(PTSD,PTSD_N)
 
##############
#### OCD #####
##############
 
#read in OCD sumstats
 OCD<-fread("N:/durable/projects/differentiation_genomics/data/sstats/ocd_aug2017",data.table=FALSE)
 
#read in information about sample size per cohort
 OCD_N<-fread("./data/cohort_info/OCDcohorts.txt",header=TRUE)
 
#calculate sample prevalence for each cohort
 OCD_N$v<-OCD_N$cases/(OCD_N$cases+OCD_N$controls)
 
#calculate cohort specific effective sample size
 OCD_N$Neff<-4*OCD_N$v*(1-OCD_N$v)*(OCD_N$cases+OCD_N$controls)
 
#calculate sum of effective sample size: 7281.307
 sum(OCD_N$Neff)
 
#add Neff column to sumstats
 OCD <- OCD %>%
    mutate(Neff = rep(7281.307,times=8409516))
 
 write.table(OCD, file = "./data/sstats/ocd_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(OCD,OCD_N)
 
##############
#### ADHD ####
##############

#read in ADHD sumstats
 ADHD<-fread("N:/durable/projects/differentiation_genomics/data/sstats/adhd_eur_jun2017",data.table=FALSE)
 
#read in information about sample size per cohort
 ADHD_N<-fread("./data/cohort_info/ADHDcohorts.txt",header=TRUE)
 
#calculate sample prevalence for each cohort
 ADHD_N$v<-ADHD_N$cases/(ADHD_N$cases+ADHD_N$controls)
 
#calculate cohort specific effective sample size
 ADHD_N$Neff<-4*ADHD_N$v*(1-ADHD_N$v)*(ADHD_N$cases+ADHD_N$controls)
 
#calculate sum of effective sample size: 46532.77
 sum(ADHD_N$Neff)
 
#add Neff column to sumstats
 ADHD <- ADHD %>%
    mutate(Neff = rep(46532.77,times=8094094))
 
 write.table(ADHD, file = "./data/sstats/adhd_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(ADHD,ADHD_N)
 
##############
##### TS #####
##############
 
#read in TS sumstats
 TS<-fread("N:/durable/projects/differentiation_genomics/data/sstats/TS_Oct2018",data.table=FALSE)
 
#read in information about sample size per cohort
 TS_N<-fread("./data/cohort_info/TScohorts.txt",header=TRUE)
 
#calculate sample prevalence for each cohort
 TS_N$v<-TS_N$cases/(TS_N$cases+TS_N$controls)
 
#calculate cohort specific effective sample size
 TS_N$Neff<-4*TS_N$v*(1-TS_N$v)*(TS_N$cases+TS_N$controls)
 
#calculate sum of effective sample size: 2891.012
 sum(TS_N$Neff)

#add Neff column to sumstats
 TS <- TS %>%
    mutate(Neff = rep(2891.012,times=8265318))
 
 write.table(TS, file = "./data/sstats/ts_withNeff.txt", sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
 
 rm(TS,TS_N)
 
################
#### Step 4 #### 
################
 
 my_pgs_names <- c("scz2022","asd","anx")

 my_pgs_caseNs <- c(74776, 18382, 31977)

 my_pgs_controlNs <- c(101023, 27969, 82114)

#read in 1000 genomes reference file used to get approximation of SNP MAF
 ref<-fread("./reference.1000G.maf.0.005.txt",data.table=FALSE)

#subset reference file to just SNP and MAF
 attach(ref)
 ref<-data.frame(SNP,MAF)
 ref <- ref %>% 
   rename(maf_1000G = MAF)

#Step 4: loop over remaining summary stats to calculate and sum effective sample sizes, 
#assuming no other info is available (i.e., using the Anx example method from here:
#https://github.com/GenomicSEM/GenomicSEM/wiki/2.1-Calculating-Sum-of-Effective-Sample-Size-and-Preparing-GWAS-Summary-Statistics)

 my_pgs <- genotools::pgs_metadata %>% 
   filter(Pheno_shortname %in% my_pgs_names) %>% 
   mutate(Pheno_shortname = factor(Pheno_shortname, levels = my_pgs_names)) %>%
   arrange(Pheno_shortname) %>%
   select(Pheno_shortname, Sumstats_filename, SNP, SE) %>%
   mutate(SE = ifelse(is.na(SE),"StdErrLogOR",SE)) %>%
   mutate(Sumstats_filename = as.factor(Sumstats_filename)) %>%
   mutate(caseN = my_pgs_caseNs,
          controlN = my_pgs_controlNs)
 
 save(my_pgs, file="./scratch/my_pgs.RData")
 load(file="./scratch/my_pgs.RData")
 
 for(pgs in my_pgs_names){
  
   this_pgs <- my_pgs %>% 
     filter(Pheno_shortname==pgs)
  
   #read in  summary stats
   sstats<-fread(paste0("./data/sstats/",this_pgs$Sumstats_filename,sep=""),data.table=FALSE)
   
   #rename columns we will use to be consistent
   sstats <- sstats %>% 
    rename( SNP =  this_pgs$SNP,
            StdErr = this_pgs$SE)
  
   #merge sstats and reference file
   sstats<-inner_join(sstats,ref,by="SNP",all=F)
  
   #calculate effective sample size implied by GWAS summary statistics 
   sstats$Neff<-4/((2*sstats$maf_1000G*(1-sstats$maf_1000G))*sstats$StdErr^2)
  
   #calculate total effective N to cap backed out Neff
   Ncases<-this_pgs$caseN 
   Ncontrols<-this_pgs$controlN 
   v<-Ncases/(Ncases+Ncontrols)
   TotalNeff<-4*v*(1-v)*(Ncases+Ncontrols)
  
   #cap at 1.1 of total effective N
   sstats$Neff<-ifelse(sstats$Neff > 1.1*TotalNeff, 1.1*TotalNeff, sstats$Neff)
  
   #lower limit of 0.5 of total effective N
   sstats$Neff<-ifelse(sstats$Neff < 0.5*TotalNeff, 0.5*TotalNeff, sstats$Neff)
  
   #remove reference panel MAF from file
   sstats$MAF<-NULL
  
   #remove sample size column so munge knows to use Neff column 
   sstats$TotalSampleSize<-NULL
  
   #output the summary stats with the SNP-specific effective sample size column
   write.table(sstats, file = paste0("./data/sstats/",pgs,"_withNeff.txt"), sep = "\t", quote=FALSE,row.names=FALSE,col.names=TRUE)
}


