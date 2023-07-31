#00.1_create_mdd_pgs.R

#load required packages
library(genotools)
vignette("genotools")

#check log file from mdd PGS for info about inputs
get_pgs_log(c("mdd2019"), 
            geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
            pgs_directory="//tsd-evs/p471/data/durable/common/pgs_directory/pgs",
            pgs_software="prsice2",
            maf="0.01",
            clump="250_1_0.1")

head(readLines(paste0("//tsd-evs/p471/data/durable/projects/differentiation_genomics/pgs_log_files/mdd2019_0.01_250_1_0.1.log")), 50)

#create a submission script for making the mdd PGS using PRSice2 on Colossus
?make_prsice()

 make_prsice(user= "Adrian",
             jobname = "mdd2019",
             outcome_type="continuous",
             cpu_time="12:00:00",
             memory="32G",
             inputs_dir="/cluster/p/p471/cluster",
             outputs_dir="/cluster/p/p471/cluster/common/raw_prsice_output/",
             sumstats_filename = "PGC_UKB_depression_genome-wide.txt",
             genotype_dir="data/genetic_data/MoBaPsychGen_v1",
             genotype_data="MoBaPsychGen_v1-ec-eur-batch-basic-qc",
             prsice_dir="/cluster/p/p471/cluster/common/prsice/",
             A1 ="A1",
             A2 = "A2",
             stat = "LogOR",
             pvalue = "P",
             snp = "MarkerName",
             thresholds = "5e-08,5e-07,5e-06,5e-05,5e-04,0.001,0.01,0.05,0.1,0.5,1",
             clump_kb = "250",
             clump_p = "1.000000",
             clump_r2 = "0.100000",
             lower = "5e-08",
             maf = "0.01",
             mhc= "exclude",
             add_metadata = TRUE,
             meta_dir = "//tsd-evs/p471/data/durable/common/pgs_directory/add_to_dir/",
             source = "https://datashare.ed.ac.uk/handle/10283/3203",
             pmid ="30718901")

 