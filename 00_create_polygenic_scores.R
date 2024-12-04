# 00_create_polygenic_scores.R

## this file creates polygenic scores for mothers, fathers, and children 
## using the genotools package, which are then merged with the phenotypic 
## data in the script: "01_data_preparation.R".

#install.packages("//ess01/p471/data/durable/common/software/genotools_0.3.0.zip", 
#         repos=NULL,
#         type = "binary")

# load necessary packages
  library(genotools)
  library(reshape2) 
  library(tidyverse)

# check which polygenic scores are available
pgs_list <- available_pgs() %>%
  pgs_search(c("adhd","asd","scz2022","bip2021","mdd2019","anx",
               "alcdep2018eur","ptsd","ocd","an2019","ts"))

# fetch polygenic scores
pgs <- fetch_pgs(
  pgs_list = c("adhd","asd","scz2022","bip2021","mdd2019","anx",
               "alcdep2018eur","ptsd","ocd","an2019","ts"),
  geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
  pgs_directory = "//ess01/P471/data/durable/common/pgs_directory/pgs/ldpred2",
  pgs_software = "ldpred2",
  pgs_meta = genotools::pgs_metadata
)

save(pgs, file="./scratch/polygenic_scores_ldpred2.RData")
load(file="./scratch/polygenic_scores_ldpred2.RData")

pgs_procd <- process_pgs(
  pgs,
  regress_pgs = TRUE,
  geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
  covs_dir = "//ess01/P471/data/durable/data/genetic/MoBaPsychGen_v1",
  return_covs = TRUE,
  analysis_type = NULL
)

pgs_kids <-  pgs_procd %>% 
  select(IID, preg_id, BARN_NR, matches("_res")) %>% 
  mutate(ind_id = paste0(preg_id,"_",BARN_NR)) %>%
  drop_na(preg_id) %>% 
  rename_with(~paste0(.x,"_child"), matches("pgs_res")) %>%
  rename_if(is.numeric, funs(str_remove(., "_pgs_res"))) %>%
  rename_with(~gsub("\\d+", "", .)) %>%
  rename("alc_child" = "alcdepeur_child") %>%
  mutate(BARN_NR=as.numeric(BARN_NR),
         an_child=an_child*-1)

pgs_dads <-  pgs_procd %>% 
  select(IID, f_id, matches("_res")) %>% 
  drop_na(f_id) %>% 
  select(f_id, matches("pgs_res")) %>% 
  rename_with(~paste0(.x,"_father"), matches("pgs_res")) %>%
  rename_if(is.numeric, funs(str_remove(., "_pgs_res"))) %>%
  rename_with(~gsub("\\d+", "", .)) %>%
  rename("alc_father" = "alcdepeur_father") %>%
  mutate(an_father=an_father*-1)

pgs_mums <-  pgs_procd %>% 
  select(IID, m_id, matches("_res")) %>% 
  drop_na(m_id) %>% 
  select(IID, m_id, matches("pgs_res")) %>% 
  rename_with(~paste0(.x,"_mother"), matches("pgs_res")) %>%
  rename_if(is.numeric, funs(str_remove(., "_pgs_res"))) %>%
  rename_with(~gsub("\\d+", "", .)) %>%
  rename("alc_mother" = "alcdepeur_mother") %>%
  mutate(an_mother=an_mother*-1) %>%
  select(-IID)

save(pgs_kids, file="./scratch/pgs_child_ldpred2.RData")
save(pgs_dads, file="./scratch/pgs_father_ldpred2.RData")
save(pgs_mums, file="./scratch/pgs_mother_ldpred2.RData")

