#01_data_preparation.R

# install packages ####

#install.packages("N:/durable/common/software/phenotools_0.2.9.zip", 
#         repos=NULL,
#         type = "binary")

#install.packages("//tsd-evs/p471/data/durable/projects/differentiation_genomics/packages/rlang_1.0.2.tar",
#                  repos = NULL,
#                  type = "source")

# load packages for data prep ####
library(tidyverse)
library(phenotools)
library(foreign)
library(data.table)
library(viridis)
library(patchwork)

# curate dataset ####
mydata <- curate_dataset(variables_required=
                           list(
                             moba=c(
                               "cbcl_ext_c_18m", "cbcl_int_c_18m",                      # CBCL 1.5yr
                               "cbcl_ext_c_3yr", "cbcl_int_c_3yr",                      # CBCL 3yr
                               "cbcl_ext_c_5yr", "cbcl_int_c_5yr",                      # CBCL 5yr
                               "ALDERRETUR_S5", "ALDERRETUR_S6",                        # child age at questionnaire return 1.5yr & 3yr
                               "AGE_RETURN_MTHS_Q5AAR", "AGE_RETURN_MTHS_Q8AAR",        # child age at questionnaire return 5yr & 8yr
                               "KJONN")),                                               # sex 
                         PDB = "2306",
                         moba_data_version = 12,
                         completion_threshold=0.5,
                         return_items=TRUE,
                         consistent_items=TRUE,
                         transformations=NULL,
                         exclusions=NULL,
                         recursive=TRUE,
                         dx_owner="child",
                         out_format="list")

save(mydata, file="./scratch/curated_dataset.RData")
load(file="./scratch/curated_dataset.RData")

myscaledata <- mydata$moba$scales
myitemdata <- mydata$moba$items

# select and rename scale variables for main dataset 
alldata <- myscaledata %>% 
  mutate(ind_id = paste0(preg_id,"_",BARN_NR)) %>%
  select(ind_id, preg_id, m_id, f_id, BARN_NR, 
         int1 = cbcl_int_c_18m, ext1 = cbcl_ext_c_18m,
         int2 = cbcl_int_c_3yr, ext2 = cbcl_ext_c_3yr,
         int3 = cbcl_int_c_5yr, ext3 = cbcl_ext_c_5yr) %>%
  as.data.frame()

covs <- myitemdata %>%
  mutate(ind_id = paste0(preg_id,"_",BARN_NR)) %>%
  select(ind_id, preg_id, m_id, f_id, BARN_NR, sex = KJONN_raw, 
         age1 = ALDERRETUR_S5_raw, age2 = ALDERRETUR_S6_raw, 
         age3 = AGE_RETURN_MTHS_Q5AAR_raw) %>%
  as.data.frame()

pheno_data <- alldata %>%
  left_join(covs)

# manipulate data ####

# filter sex and recode to dummy variable (0 = male, 1 = female)
pheno_data <- pheno_data %>% 
  mutate(sex = case_when(sex==1~0,sex==2~1))

# re-scale child age at questionnaire return
pheno_data <- pheno_data %>%
  mutate(age1 = round(age1/30)) %>%
  mutate(age2 = round(age2/30))

# scale CBCL scores
pheno_data <- pheno_data %>% 
  mutate_at(vars(matches("int|ext")), list(~scale(.)))

# create and scale differentiation scores
pheno_data <- pheno_data %>%
  mutate(diff1 = scale(ext1 - int1)) %>%
  mutate(diff2 = scale(ext2 - int2)) %>%
  mutate(diff3 = scale(ext3 - int3))

# create and scale total scores
pheno_data <- pheno_data %>%
  mutate(tot1 = scale(ext1 + int1)) %>%
  mutate(tot2 = scale(ext2 + int2)) %>%
  mutate(tot3 = scale(ext3 + int3))

# select sample ####

# Restrict to individuals with non-missing on at
# least one observed cbcl variables

pheno_data <- pheno_data %>% 
  filter(!is.na(diff1)|!is.na(diff2)|!is.na(diff3))

# write out processed phenotypic dataset ####
save(pheno_data, file = './data/processed_pheno_data.RData')
load(file = './data/processed_pheno_data.RData')

# select variables
alldata <- pheno_data %>%
  select(ind_id, preg_id, m_id, f_id, BARN_NR, sex,
         diff1, tot1, diff2, tot2, diff3, tot3,
         age1, age2, age3) %>%
  as.data.frame()

#create phenotype and covariate files for GWAS ####
linkage_IDs <- fread("N:/durable/data/genetic/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt")
  
covar_gwas_1.5y <- merge(pheno_data, linkage_IDs, by.x="ind_id", by.y="ID_2306", all.x=T) %>%
  select(FID,IID,sex,age1,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,
         batch_harvest12a,batch_harvest12b,batch_harvest24,batch_rotterdam1,
         batch_rotterdam2,batch_adhd1,batch_adhd2,batch_norment_may2016,
         batch_norment_feb2018,batch_norment_feb2020_v1,batch_norment_feb2020_v3,
         batch_norment_aug2020_996,batch_norment_aug2020_1029,
         batch_norment_nov2020_1066,batch_norment_nov2020_1077,
         batch_norment_nov2020_1108,batch_norment_nov2020_1109,
         batch_norment_nov2020_1135,batch_norment_nov2020_1146,
         batch_norment_mar2021_1273,batch_norment_mar2021_1409,
         batch_norment_mar2021_1413,batch_norment_mar2021_1531) %>%
  na.omit()

covar_gwas_3y <- merge(pheno_data, linkage_IDs, by.x="ind_id", by.y="ID_2306", all.x=T) %>%
  select(FID,IID,sex,age2,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,
         batch_harvest12a,batch_harvest12b,batch_harvest24,batch_rotterdam1,
         batch_rotterdam2,batch_adhd1,batch_adhd2,batch_norment_may2016,
         batch_norment_feb2018,batch_norment_feb2020_v1,batch_norment_feb2020_v3,
         batch_norment_aug2020_996,batch_norment_aug2020_1029,
         batch_norment_nov2020_1066,batch_norment_nov2020_1077,
         batch_norment_nov2020_1108,batch_norment_nov2020_1109,
         batch_norment_nov2020_1135,batch_norment_nov2020_1146,
         batch_norment_mar2021_1273,batch_norment_mar2021_1409,
         batch_norment_mar2021_1413,batch_norment_mar2021_1531) %>%
  na.omit()

covar_gwas_5y <- merge(pheno_data, linkage_IDs, by.x="ind_id", by.y="ID_2306", all.x=T) %>%
  select(FID,IID,sex,age3,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,
         batch_harvest12a,batch_harvest12b,batch_harvest24,batch_rotterdam1,
         batch_rotterdam2,batch_adhd1,batch_adhd2,batch_norment_may2016,
         batch_norment_feb2018,batch_norment_feb2020_v1,batch_norment_feb2020_v3,
         batch_norment_aug2020_996,batch_norment_aug2020_1029,
         batch_norment_nov2020_1066,batch_norment_nov2020_1077,
         batch_norment_nov2020_1108,batch_norment_nov2020_1109,
         batch_norment_nov2020_1135,batch_norment_nov2020_1146,
         batch_norment_mar2021_1273,batch_norment_mar2021_1409,
         batch_norment_mar2021_1413,batch_norment_mar2021_1531) %>%
  na.omit()

pheno_gwas_1.5y <- merge(pheno_data, linkage_IDs, by.x="ind_id", by.y="ID_2306", all.x=T) %>%
  select(FID,IID,diff1,tot1) %>%
  mutate(diff1 = as.numeric(diff1)) %>%
  mutate(tot1 = as.numeric(tot1)) %>%
  na.omit()

pheno_gwas_3y <- merge(pheno_data, linkage_IDs, by.x="ind_id", by.y="ID_2306", all.x=T) %>%
  select(FID,IID,diff2,tot2) %>%
  mutate(diff2 = as.numeric(diff2)) %>%
  mutate(tot2 = as.numeric(tot2)) %>%
  na.omit()

pheno_gwas_5y <- merge(pheno_data, linkage_IDs, by.x="ind_id", by.y="ID_2306", all.x=T) %>%
  select(FID,IID,diff3,tot3) %>%
  mutate(diff3 = as.numeric(diff3)) %>%
  mutate(tot3 = as.numeric(tot3)) %>%
  na.omit()

write_delim(covar_gwas_1.5y,file="covar_gwas_1.5y.txt",col_names=TRUE)
write_delim(covar_gwas_3y,file="covar_gwas_3y.txt",col_names=TRUE)
write_delim(covar_gwas_5y,file="covar_gwas_5y.txt",col_names=TRUE)

write_delim(pheno_gwas_1.5y,file="pheno_gwas_1.5y.txt",col_names=TRUE)
write_delim(pheno_gwas_3y,file="pheno_gwas_3y.txt",col_names=TRUE)
write_delim(pheno_gwas_5y,file="pheno_gwas_5y.txt",col_names=TRUE)

#merge phenotypic and genotypic data ####

##read in PGS data from the script: 
##00.1_create_polygenic_scores.R

load(file="./scratch/pgs_child_ldpred2.RData")
load(file="./scratch/pgs_father_ldpred2.RData")
load(file="./scratch/pgs_mother_ldpred2.RData")

fulldata <- alldata %>%
  left_join(pgs_kids, by = c("ind_id","preg_id","BARN_NR")) %>%
  left_join(pgs_dads, by = "f_id") %>%
  left_join(pgs_mums, by = "m_id") %>%
  mutate_at(vars(matches("_child|_mother|_father")), list(~scale(.)))

save(fulldata, file = './data/processed_data_ld.RData')
load(file = './data/processed_data_ld.RData')

## create Figure 1 illustrating calculation of outcomes

# load data with ext and int scores
load(file = './data/processed_pheno_data.RData')

# select ext and int, and drop NAs
dat <- pheno_data %>%
  select(ind_id, Behavioral=ext1, Emotional=int1) %>%
  na.omit()

# calculate differentiation and total scores
dat$Differentiation <- scale(dat$Behavioral) - scale(dat$Emotional)
dat$`Total problems` <- scale(dat$Behavioral) + scale(dat$Emotional)

plotdat <- dat %>%
  gather("CBCL_var","value",-ind_id,-Differentiation,-`Total problems`)

# select 80 random unique 'ind_id's and assign new identifiers
unique_ind_ids <- plotdat %>%
  distinct(ind_id) %>%
  sample_n(80) %>%
  mutate(new_ind_id = as.character(row_number()))

# join back to the original data
plotdat_80 <- plotdat %>%
  inner_join(unique_ind_ids, by = "ind_id") %>%
  arrange(as.numeric(new_ind_id))

#panel A: cleveland dot plot
p1 <- ggplot(plotdat_80, aes(value, as.numeric(new_ind_id)))+
  geom_line(aes(group = as.numeric(new_ind_id), colour=Differentiation), size = 1.2) +
  geom_point(aes(shape = CBCL_var), fill = "white", size = 2.2) +
  scale_shape_manual(values = c(24, 22), guide = guide_legend(title = NULL)) +
  scale_colour_viridis(option="inferno") +
  guides(shape=guide_legend(order = 1, title = NULL, reverse = TRUE, label.theme = element_text(size=17)))+
  theme_minimal() +
  ylab("Randomly selected individuals") +
  xlab("CBCL subscale scores") +
  geom_text(aes(x = Inf, y = -Inf), label = "Positive = more \nbehavioral           ",
            hjust = -2.23, vjust = 2.7, size = 4.4) +
  geom_text(aes(x = Inf, y = -Inf), label = "Negative = more \nemotional                                                 ",
            hjust = -0.13, vjust = 2.7, size = 4.4) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.margin = margin(12, 97, 0, 8),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=15, colour="black"),
    axis.text = element_text(size = 13, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(clip = "off")

p1

# select 3000 random unique 'ind_id's and assign new identifiers
unique_ind_ids <- plotdat %>%
  distinct(ind_id) %>%
  sample_n(3000) %>%
  mutate(new_ind_id = as.character(row_number()))

# join back to the original data
plotdat_3000 <- plotdat %>%
  inner_join(unique_ind_ids, by = "ind_id") %>%
  arrange(as.numeric(new_ind_id))

# panel 2: histogram differentiation
p2 <- ggplot(plotdat_3000, aes(Differentiation))+
  geom_histogram(aes(y=..density..,fill=..x..),colour= "black",binwidth = 0.5, show.legend = FALSE) +
  geom_density(colour="black" )+
  scale_fill_viridis(option="inferno")+
  guides(fill="none") +
  theme_minimal() +
  ylab("") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=15, colour="black"),
        axis.text=element_text(size=13, colour="black"),
        axis.line=element_line(colour="black"))

p2

# panel 3: histogram total problems
p3 <- ggplot(plotdat_3000, aes(`Total problems`))+
  geom_histogram(aes(y=..density..),fill="grey80",colour= "black",binwidth = 0.6) +
  geom_density(colour="black" )+
  theme_minimal() +
  ylab("") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=15, colour="black"),
        axis.text=element_text(size=13, colour="black"),
        axis.line=element_line(colour="black")) +
  guides(fill = "none")

p3

#panel 4: dot plot correlation differentiation and total problems
p4 <- ggplot(plotdat_3000) +
  geom_jitter(aes(Differentiation, `Total problems`, fill=Differentiation), shape=21, size = 1.5, width=0.75, height=0.75)+
  geom_smooth(aes(Differentiation, `Total problems`), method="lm", colour="black")+
  scale_fill_viridis(option="inferno")+
  theme_minimal() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=15, colour="black"),
        axis.text=element_text(size=13, colour="black"),
        axis.line=element_line(colour="black")) +
  guides(fill = "none")

p4

patch <- p1 + (p2 / p3 / p4)

patch2 <- patch + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

patch2

# save out
ggsave("figures/Figure_1.tiff", device="tiff", dpi=320, units="cm",
     height = 20, width = 20)

