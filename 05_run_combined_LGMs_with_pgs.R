#05_run_combined_LGMs_with_pgs.R

## This script runs the latent growth models (LGMs) with PGS by sourcing:
## 05.1_specify_combined_LGMs_with_pgs.R

library(tidyverse)
library(lavaan)
library(patchwork)
library(ggrepel)
library(viridis)

# read in processed data from 01_data_preparation.R, and
# trio data from 06_run_LGMs_with_trio_pgs.R
load("./data/processed_data.RData")
load(file="./data/trio_full.RData")

# specify combined LGMs
source("./scripts/05.1_specify_combined_LGMs_with_pgs.R")

# run basic LGM without predictors
fit_model_basic <- sem(model_basic, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = fulldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model_basic, fit.measures = TRUE, std = TRUE)

# run LGM with predictors - both intercept and slope for diff and tot
fit_model1_both <- sem(model1, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = fulldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model1_both, fit.measures = TRUE, std = TRUE)

save(fit_model1_both, file= "./output/fit_model1_both.RData")
load("./output/fit_model1_both.RData")

# run LGM with predictors - intercepts only for diff and tot
fit_model2_both <- sem(model2, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = fulldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model2_both, fit.measures = TRUE, std = TRUE)

save(fit_model2_both, file= "./output/fit_model2_both.RData")
load("./output/fit_model2_both.RData")

# run LGM with predictors - slopes only for diff and tot
fit_model3_both <- sem(model3, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = fulldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model3_both, fit.measures = TRUE, std = TRUE)

save(fit_model3_both, file= "./output/fit_model3_both.RData")
load("./output/fit_model3_both.RData")

# compare the different models
intercepts_only <- anova(fit_model1_both, fit_model2_both)
slopes_only <- anova(fit_model1_both, fit_model3_both)

intercepts_only 
slopes_only 

# run LGM with predictors in trios - both intercept and slope for diff and tot
fit_model1_trios <- sem(model1, 
                        missing = "ML", 
                        estimator = "MLR", 
                        data = trio_full, 
                        cluster = "m_id",
                        fixed.x = F)

# provide output for the model
summary(fit_model1_trios, fit.measures = TRUE, std = TRUE)

save(fit_model1_trios, file= "./output/fit_model1_trios.RData")
load("./output/fit_model1_trios.RData")

# take intercept estimates from main model
tmp <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(lhs=="i1"|lhs=="i2",
         op == "~")

# remove sex from predictors
ests <- tmp[-c(1,2), ]

# adjust pvalues
ests$FDR_pval <- p.adjust(ests$pvalue, method="fdr")

# rename and make factor
ests <- ests %>%
  mutate(outcome = as.factor(lhs),
         pgs = as.factor(rhs))

# rename factor levels
levels(ests$outcome)[levels(ests$outcome)=="i1"] <- "Differentiation"
levels(ests$outcome)[levels(ests$outcome)=="i2"] <- "Total problems"

levels(ests$pgs)[levels(ests$pgs)=="adhd_child"] <- "ADHD"
levels(ests$pgs)[levels(ests$pgs)=="alc_child"] <- "Alcohol"
levels(ests$pgs)[levels(ests$pgs)=="an_child"] <- "Anorexia"
levels(ests$pgs)[levels(ests$pgs)=="anx_child"] <- "Anxiety"
levels(ests$pgs)[levels(ests$pgs)=="asd_child"] <- "Autism"
levels(ests$pgs)[levels(ests$pgs)=="bip_child"] <- "Bipolar"
levels(ests$pgs)[levels(ests$pgs)=="scz_child"] <- "SCZ"
levels(ests$pgs)[levels(ests$pgs)=="mdd_child"] <- "MDD"
levels(ests$pgs)[levels(ests$pgs)=="ocd_child"] <- "OCD"
levels(ests$pgs)[levels(ests$pgs)=="ptsd_child"] <- "PTSD"
levels(ests$pgs)[levels(ests$pgs)=="ts_child"] <- "Tourette's"

# fix order of factors
ests <- ests %>%
  mutate(outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

# save out
save(ests, file="./output/unadjusted_PGS_ests.RData")
load(file="./output/unadjusted_PGS_ests.RData")

# take intercept estimates from model
tmp_trios <- standardizedSolution(fit_model1_trios, ci = TRUE, level = 0.95) %>%
  filter(lhs=="i1"|lhs=="i2",
         op == "~")

# remove sex from predictors
unadj_ests_trio <- tmp_trios[-c(1,2), ]

# adjust pvalues
unadj_ests_trio$FDR_pval <- p.adjust(unadj_ests_trio$pvalue, method="fdr")

# rename and make factor
unadj_ests_trio <- unadj_ests_trio %>%
  mutate(outcome = as.factor(lhs),
         pgs = as.factor(rhs))

# rename factor levels
levels(unadj_ests_trio$outcome)[levels(unadj_ests_trio$outcome)=="i1"] <- "Differentiation"
levels(unadj_ests_trio$outcome)[levels(unadj_ests_trio$outcome)=="i2"] <- "Total problems"

levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="adhd_child"] <- "ADHD"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="alc_child"] <- "Alcohol"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="an_child"] <- "Anorexia"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="anx_child"] <- "Anxiety"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="asd_child"] <- "Autism"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="bip_child"] <- "Bipolar"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="scz_child"] <- "SCZ"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="mdd_child"] <- "MDD"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="ocd_child"] <- "OCD"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="ptsd_child"] <- "PTSD"
levels(unadj_ests_trio$pgs)[levels(unadj_ests_trio$pgs)=="ts_child"] <- "Tourette's"

# fix order of factors
unadj_ests_trio <- unadj_ests_trio %>%
  mutate(outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

# save out
save(unadj_ests_trio, file="./output/unadjusted_PGS_ests_trio_sample.RData")
load(file="./output/unadjusted_PGS_ests_trio_sample.RData")

# take slope estimates from model
tmp2 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(lhs=="s1"|lhs=="s2",
         op == "~")

# remove sex from predictors
ests2 <- tmp2[-c(1,2), ]

# adjust pvalues
ests2$FDR_pval <- p.adjust(ests2$pvalue, method="fdr")

# rename and make factor
ests2 <- ests2 %>%
  mutate(outcome = as.factor(lhs),
         pgs = as.factor(rhs))

# rename factor levels
levels(ests2$outcome)[levels(ests2$outcome)=="s1"] <- "Differentiation"
levels(ests2$outcome)[levels(ests2$outcome)=="s2"] <- "Total problems"

levels(ests2$pgs)[levels(ests2$pgs)=="adhd_child"] <- "ADHD"
levels(ests2$pgs)[levels(ests2$pgs)=="alc_child"] <- "Alcohol"
levels(ests2$pgs)[levels(ests2$pgs)=="an_child"] <- "Anorexia"
levels(ests2$pgs)[levels(ests2$pgs)=="anx_child"] <- "Anxiety"
levels(ests2$pgs)[levels(ests2$pgs)=="asd_child"] <- "Autism"
levels(ests2$pgs)[levels(ests2$pgs)=="bip_child"] <- "Bipolar"
levels(ests2$pgs)[levels(ests2$pgs)=="scz_child"] <- "SCZ"
levels(ests2$pgs)[levels(ests2$pgs)=="mdd_child"] <- "MDD"
levels(ests2$pgs)[levels(ests2$pgs)=="ocd_child"] <- "OCD"
levels(ests2$pgs)[levels(ests2$pgs)=="ptsd_child"] <- "PTSD"
levels(ests2$pgs)[levels(ests2$pgs)=="ts_child"] <- "Tourette's"

# fix order of factors
ests2 <- ests2 %>%
  mutate(outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

# save out
save(ests2, file="./output/unadjusted_PGS_ests_slope.RData")
load(file="./output/unadjusted_PGS_ests_slope.RData")

# create tables intercept and slope outcomes ----

# Table Sx: create table unadjusted PGS effects on intercept
tbl_int_unadj <- ests %>%
  mutate(across(where(is.numeric), round, digits=2)) %>%
  select(outcome,pgs,est.std,ci.lower,ci.upper,FDR_pval) %>%
  mutate(outcome = if_else(outcome == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = outcome, 
              values_from = c(est.std, ci.lower, ci.upper, FDR_pval),
              names_sep = ".") %>%
  select(pgs, ends_with(".d"), ends_with(".t"))

(ft_tbl_int_unadj <- tbl_int_unadj %>% 
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.std.d="EST",
                                  ci.lower.d="LCI",ci.upper.d="UCI",
                                  FDR_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",
                                  FDR_pval.t="")) %>%
    add_header_row(values=list("","Differentiation","Total problems"), colwidths = c(1,4,4)) %>%
    add_header_lines(values="Table Sx: Unadjusted polygenic score associations (intercept)") %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=3,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "center", part="all") %>% 
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_unadj, path = "./tables/Table_Sx_unadjusted_PGS.docx")

# Table Sx: create table unadjusted PGS effects on slope
tbl_slope_unadj <- ests2 %>%
  mutate(across(where(is.numeric), round, digits=2)) %>%
  select(outcome,pgs,est.std,ci.lower,ci.upper,FDR_pval) %>%
  mutate(outcome = if_else(outcome == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = outcome, 
              values_from = c(est.std, ci.lower, ci.upper, FDR_pval),
              names_sep = ".") %>%
  select(pgs, ends_with(".d"), ends_with(".t"))

(ft_tbl_slope_unadj <- tbl_slope_unadj %>% 
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.std.d="EST",
                                  ci.lower.d="LCI",ci.upper.d="UCI",
                                  FDR_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",
                                  FDR_pval.t="")) %>%
    add_header_row(values=list("","Differentiation","Total problems"), colwidths = c(1,4,4)) %>%
    add_header_lines(values="Table Sx: Unadjusted polygenic score associations (slope)") %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=3,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "center", part="all") %>% 
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_slope_unadj, path = "./tables/Table_Sx_unadjusted_PGS_slope.docx")
