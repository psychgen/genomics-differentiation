# 00.1_create_polygenic_scores.R

## this file creates polygenic scores for mothers, fathers, and children 
## using the genotools package, which are then merged with the phenotypic 
## data in the script: "01_data_preparation.R".

# load necessary packages
  library(genotools)
  library(corrplot)
  library(reshape2) 
  library(tidyverse)

# check which polygenic scores are available

pgs_list <- available_pgs(
            geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
            pgs_directory = "//ess01/p471/data/durable/common/pgs_directory/pgs",
            pgs_software = "prsice2",
            pgs_meta = genotools::pgs_metadata)

# fetch polygenic scores

pgs <- fetch_pgs(
  pgs_list = c("adhd","asd","scz2022","bip2021","mdd2019","anx",
               "alcdep2018eur","ptsd","ocd","an2019","ts"),
  thresholds = c(5e-08, 5e-07, 5e-06, 5e-05, 5e-04, 0.001, 0.01, 0.05, 0.1, 0.5, 1),
  threshold_range = FALSE,
  geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
  pgs_directory = "//ess01/p471/data/durable/common/pgs_directory/pgs",
  pgs_software = "prsice2",
  pgs_meta = genotools::pgs_metadata,
  maf = "0.01",
  clump = "250_1_0.1"
)

save(pgs, file="./scratch/polygenic_scores.RData")
load(file="./scratch/polygenic_scores.RData")

pgs_procd <- process_pgs(
  pgs,
  regress_pgs = TRUE,
  geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
  covs_dir = "//ess01/p471/data/durable/data/genetic/MoBaPsychGen_v1",
  return_covs = TRUE,
  analysis_type = NULL
)

pgs_pcs_kids <-  pgs_procd %>% 
  select(IID, preg_id, BARN_NR, matches("_res")) %>% 
  mutate(ind_id = paste0(preg_id,"_",BARN_NR)) %>%
  drop_na(preg_id) %>% 
  pgs_pca(indid = "IID",
          pgs_var_stem = c("adhd","asd","scz2022","bip2021","mdd2019","anx",
                           "alcdep2018eur","ptsd","ocd","an2019","ts")) %>% 
  select(ind_id,preg_id, BARN_NR, matches("pgs.pc")) %>% 
  rename_with(~paste0(.x,"_child"), matches("pgs.pc")) %>%
  rename_if(is.numeric, funs(str_remove(., ".pgs.pc"))) %>%
  rename_with(~gsub("\\d+", "", .)) %>%
  rename("alc_child" = "alcdepeur_child") %>%
  mutate(BARN_NR=as.numeric(BARN_NR))

pgs_pcs_dads <-  pgs_procd %>% 
  select(IID, f_id, matches("_res")) %>% 
  drop_na(f_id) %>% 
  pgs_pca(indid = "IID",
          pgs_var_stem = c("adhd","asd","scz2022","bip2021","mdd2019","anx",
                           "alcdep2018eur","ptsd","ocd","an2019","ts")) %>% 
  select(f_id, matches("pgs.pc")) %>% 
  rename_with(~paste0(.x,"_father"), matches("pgs.pc")) %>%
  rename_if(is.numeric, funs(str_remove(., ".pgs.pc"))) %>%
  rename_with(~gsub("\\d+", "", .)) %>%
  rename("alc_father" = "alcdepeur_father")

pgs_pcs_mums <-  pgs_procd %>% 
  select(IID, m_id, matches("_res")) %>% 
  drop_na(m_id) %>% 
  pgs_pca(indid = "IID",
          pgs_var_stem = c("adhd","asd","scz2022","bip2021","mdd2019","anx",
                           "alcdep2018eur","ptsd","ocd","an2019","ts")) %>% 
  select(IID, m_id, matches("pgs.pc")) %>% 
  rename_with(~paste0(.x,"_mother"), matches("pgs.pc")) %>%
  rename_if(is.numeric, funs(str_remove(., ".pgs.pc"))) %>%
  rename_with(~gsub("\\d+", "", .)) %>%
  rename("alc_mother" = "alcdepeur_mother")

save(pgs_pcs_kids, file="./scratch/pca_child.RData")
save(pgs_pcs_dads, file="./scratch/pca_father.RData")
save(pgs_pcs_mums, file="./scratch/pca_mother.RData")

