#06_run_trio_pgs_analyses.R

#This script runs LGMs with trio PGS as predictors. The models tested here are: 

## 1. multivariate PGS model in the full sample of trios
## 2. multivariate PGS model restricted to unrelated trios
## 3. univariate PGS models in the full sample of trios

#Results are then processed, plotted, and tabulated for each of these models,
#which are run by sourcing the script: '06.1_specify_LGMs_with_trio_pgs.R'.

library(tidyverse)
library(lavaan)
library(scales)

# load processed data and trio data
load("./data/processed_data_ld.RData")
load(file="./data/trio_full_ld.RData")
load(file="./data/trio_unrelated_ld.RData")

# specify trio PGS model
source("./scripts/06.1_specify_LGMs_with_trio_pgs.R")

# run multivariate trio PGS model on full sample (with clustering on 
# maternal id to account for child siblings as the main source of relatedness)
fit_triomodel_full <- sem(triomodel, 
                          missing = "ML", 
                          estimator = "MLR", 
                          data = trio_full, 
                          cluster = "m_id",
                          fixed.x = F)

# provide output for the model (full sample)
summary(fit_triomodel_full, fit.measures = TRUE, std = TRUE, rsquare=TRUE)

# save / load trio PGS model output (full sample)
save(fit_triomodel_full, file= "./output/fit_triomodel_full_ld.RData")
load("./output/fit_triomodel_full.RData")

# run trio PGS model diff + tot on unrelated sample (sensitivity analysis)
fit_triomodel_unrelated <- sem(triomodel, 
                               missing = "ML", 
                               estimator = "MLR", 
                               data = trio_unrelated_ld, 
                               fixed.x = F)

# provide output for the model (sensitivity)
summary(fit_triomodel_unrelated, fit.measures = TRUE, std = TRUE, rsquare=TRUE)

# save / load trio PGS model output (unrelated sensitivity analysis)
save(fit_triomodel_unrelated, file= "./output/fit_triomodel_unrelated_ld.RData")
load("./output/fit_triomodel_unrelated_ld.RData")

# loop to run univariate trio PGS models on full sample (with clustering on 
#maternal id to account for child siblings as the main source of relatedness)
pgss <- c("an","ocd","ts","scz","bip","alc","adhd","asd","ptsd","mdd","anx")
uni_trio_output <- list()
fit_uni_triomodel_full <- list()

for(pgs in pgss){
  
  message(paste0("Running model for ",pgs))
  
  trio_full$pgs_child <- trio_full[,paste0(pgs,"_child")]
  trio_full$pgs_mother <- trio_full[,paste0(pgs,"_mother")]
  trio_full$pgs_father <- trio_full[,paste0(pgs,"_father")]
  
  # run all the models, saving fit objects as list elements
  
  fit_uni_triomodel_full[[pgs]] <- sem(uni_triomodel, 
                                       missing = "ML", 
                                       estimator = "MLR", 
                                       data = trio_full, 
                                       cluster = "m_id",
                                       fixed.x = F)
  
  uni_trio_output[[pgs]] <- summary(fit_uni_triomodel_full[[pgs]], 
                                    fit.measures = TRUE, 
                                    std = TRUE, 
                                    rsquare=TRUE)

}

uni_trio_output

fit_uni_triomodel_full

# save / load trio univariate PGS model output (full sample)
save(fit_uni_triomodel_full, file= "./output/fit_uni_triomodel_full.RData")
load("./output/fit_uni_triomodel_full.RData")

# take diff estimates from full sample trio PGS model (univariate)
uni_trio_diff_results <- fit_uni_triomodel_full %>%
  purrr::map_dfr(~as.data.frame(standardizedSolution(.x,ci=TRUE,level=0.95)),.id="pgs") %>%
  filter(lhs=="i1"|lhs=="s1",
         op=="~", rhs!="sex") %>%
  mutate(model="Differentiation")

# adjust p-values intercept
uni_trio_diff_results <- uni_trio_diff_results %>%
  filter(lhs=="i1") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# take tot estimates from full sample trio PGS model (univariate)
uni_trio_tot_results <- fit_uni_triomodel_full %>%
  purrr::map_dfr(~as.data.frame(standardizedSolution(.x,ci=TRUE,level=0.95)),.id="pgs") %>%
  filter(lhs=="i2"|lhs=="s2",
         op=="~", rhs!="sex") %>%
  mutate(model="Total problems")

# adjust p-values intercept
uni_trio_tot_results <- uni_trio_tot_results %>%
  filter(lhs=="i2") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# merge diff and tot, create effect variable 
# distinguishing direct and indirect effects
uni_trio_both <- uni_trio_diff_results %>%
  full_join(uni_trio_tot_results) %>%
  mutate(Effect = case_when(str_detect(rhs, "_child") ~ "Direct\n (cPGS)",
                            !str_detect(rhs, "_child") ~ "Indirect\n (mPGS + fPGS)"),
         Effect = fct_relevel(Effect,"Direct\n (cPGS)","Indirect\n (mPGS + fPGS)"),
         model = as.factor(model),
         pgs = as.factor(pgs))

levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="adhd"] <- "ADHD"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="alc"] <- "Alcohol"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="an"] <- "Anorexia"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="anx"] <- "Anxiety"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="asd"] <- "Autism"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="bip"] <- "Bipolar"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="scz"] <- "SCZ"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="mdd"] <- "MDD"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="ocd"] <- "OCD"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="ptsd"] <- "PTSD"
levels(uni_trio_both$pgs)[levels(uni_trio_both$pgs)=="ts"] <- "Tourette's"

# save out full sample results
save(uni_trio_both,file="./output/univariate_trio_PGS_effects.RData")
load(file="./output/univariate_trio_PGS_effects.RData")

# create table direct genetic effects on intercept (univariate)
tbl_int_uni_trio_direct <- uni_trio_both %>%
  filter(Effect=="Direct\n (cPGS)") %>%
  mutate(across(where(is.numeric), round, digits=2)) %>%
  select(model,pgs,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(pgs, ends_with(".d"), ends_with(".t")) %>%
  mutate(pgs=as.factor(pgs)) %>%
  mutate(pgs=fct_relevel(pgs,"ADHD","Autism","SCZ","Bipolar","MDD",
                         "Anxiety","Alcohol","PTSD","OCD","Anorexia","Tourette's")) %>%
  arrange(pgs)

rows_to_replace_col5 <- which(tbl_int_uni_trio_direct[[5]] == 0)
rows_to_replace_col9 <- which(tbl_int_uni_trio_direct[[9]] == 0)

(ft_tbl_int_uni_trio_direct <- tbl_int_uni_trio_direct %>% 
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Differentiation","Total problems"), colwidths = c(1,4,4)) %>%
    compose(i = c(2,2), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    compose(i = rows_to_replace_col5, j = 5, part = "body",value = as_paragraph("<0.01")) %>%
    compose(i = rows_to_replace_col9, j = 9, part = "body",value = as_paragraph("<0.01")) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 1, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_uni_trio_direct, path = "./tables/Table_S12_univariate_trio_direct_effects.docx")

# create table indirect genetic effects on intercept (univariate)
tbl_int_uni_trio_indirect <- uni_trio_both %>%
  filter(Effect=="Indirect\n (mPGS + fPGS)") %>%
  mutate(across(where(is.numeric), round, digits=2),
         parent = ifelse(str_detect(rhs,"mother"),"Mother","Father")) %>%
  select(model,pgs,parent,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(parent, pgs, ends_with(".d"), ends_with(".t")) %>%
  mutate(parent = as.factor(parent)) %>%
  mutate(parent = fct_relevel(parent,"Mother","Father")) %>%
  arrange(parent) %>% 
  mutate(pgs=as.factor(pgs)) %>%
  mutate(pgs=fct_relevel(pgs,"ADHD","Autism","SCZ","Bipolar","MDD",
                         "Anxiety","Alcohol","PTSD","OCD","Anorexia","Tourette's"))

(ft_tbl_int_uni_trio_indirect <- tbl_int_uni_trio_indirect %>% 
    flextable() %>% 
    set_header_labels(values=list(parent="PARENT",pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","","Differentiation","Total problems"), colwidths = c(1,1,4,4)) %>%
    compose(i = c(2,2), j = c(6,10), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    merge_v(j = ~ parent) %>%
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 2, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_uni_trio_indirect, path = "./tables/Table_S13_univariate_trio_indirect_effects.docx")

# take diff estimates from full sample trio PGS model (multivariate)
trio_diff <- standardizedSolution(fit_triomodel_full, ci=TRUE, level=0.95) %>%
  filter(lhs=="i1"|lhs=="s1",
         op == "~") %>%
  mutate(model = "Differentiation")
fit_triomodel_unrelated

trio_diff <- trio_diff[-c(1,2),]

# adjust p-values for intercept
trio_diff_int <- trio_diff %>%
  filter(lhs=="i1") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# adjust p-values for slope
trio_diff_slope <- trio_diff %>%
  filter(lhs=="s1") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# take tot estimates from full sample trio PGS model
trio_tot <- standardizedSolution(fit_triomodel_full, ci=TRUE, level=0.95) %>%
  filter(lhs=="i2"|lhs=="s2",
         op == "~") %>%
  mutate(model = "Total problems")

trio_tot <- trio_tot[-c(1,2),]

# adjust p-values for intercept
trio_tot_int <- trio_tot %>%
  filter(lhs=="i2") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# adjust p-values for slope
trio_tot_slope <- trio_tot %>%
  filter(lhs=="s2") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# merge diff and tot, create effect variable 
# distinguishing direct and indirect effects
trio_both <- trio_diff_int %>%
  full_join(trio_tot_int) %>%
  mutate(Effect = case_when(str_detect(rhs, "_child") ~ "Direct\n (cPGS)",
                            !str_detect(rhs, "_child") &
                              model == "Total problems" |
                              model == "Differentiation" ~ "Indirect\n (mPGS + fPGS)"),
         Effect = fct_relevel(Effect,"Direct\n (cPGS)","Indirect\n (mPGS + fPGS)"),
         model = as.factor(model),
         pgs = as.factor(rhs))

levels(trio_both$pgs)[levels(trio_both$pgs)=="adhd_child"] <- "ADHD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="alc_child"] <- "Alcohol"
levels(trio_both$pgs)[levels(trio_both$pgs)=="an_child"] <- "Anorexia"
levels(trio_both$pgs)[levels(trio_both$pgs)=="anx_child"] <- "Anxiety"
levels(trio_both$pgs)[levels(trio_both$pgs)=="asd_child"] <- "Autism"
levels(trio_both$pgs)[levels(trio_both$pgs)=="bip_child"] <- "Bipolar"
levels(trio_both$pgs)[levels(trio_both$pgs)=="scz_child"] <- "SCZ"
levels(trio_both$pgs)[levels(trio_both$pgs)=="mdd_child"] <- "MDD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ocd_child"] <- "OCD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ptsd_child"] <- "PTSD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ts_child"] <- "Tourette's"

levels(trio_both$pgs)[levels(trio_both$pgs)=="adhd_mother"] <- "ADHD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="alc_mother"] <- "Alcohol"
levels(trio_both$pgs)[levels(trio_both$pgs)=="an_mother"] <- "Anorexia"
levels(trio_both$pgs)[levels(trio_both$pgs)=="anx_mother"] <- "Anxiety"
levels(trio_both$pgs)[levels(trio_both$pgs)=="asd_mother"] <- "Autism"
levels(trio_both$pgs)[levels(trio_both$pgs)=="bip_mother"] <- "Bipolar"
levels(trio_both$pgs)[levels(trio_both$pgs)=="scz_mother"] <- "SCZ"
levels(trio_both$pgs)[levels(trio_both$pgs)=="mdd_mother"] <- "MDD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ocd_mother"] <- "OCD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ptsd_mother"] <- "PTSD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ts_mother"] <- "Tourette's"

levels(trio_both$pgs)[levels(trio_both$pgs)=="adhd_father"] <- "ADHD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="alc_father"] <- "Alcohol"
levels(trio_both$pgs)[levels(trio_both$pgs)=="an_father"] <- "Anorexia"
levels(trio_both$pgs)[levels(trio_both$pgs)=="anx_father"] <- "Anxiety"
levels(trio_both$pgs)[levels(trio_both$pgs)=="asd_father"] <- "Autism"
levels(trio_both$pgs)[levels(trio_both$pgs)=="bip_father"] <- "Bipolar"
levels(trio_both$pgs)[levels(trio_both$pgs)=="scz_father"] <- "SCZ"
levels(trio_both$pgs)[levels(trio_both$pgs)=="mdd_father"] <- "MDD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ocd_father"] <- "OCD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ptsd_father"] <- "PTSD"
levels(trio_both$pgs)[levels(trio_both$pgs)=="ts_father"] <- "Tourette's"

# save out full sample results
save(trio_both,file="./output/trio_PGS_effects.RData")

# take diff estimates from unrelated sample trio PGS model
trio_diff_unr <- standardizedSolution(fit_triomodel_unrelated, ci=TRUE, level=0.95) %>%
  filter(lhs=="i1"|lhs=="s1",
         op == "~") %>%
  mutate(model = "Differentiation")

trio_diff_unr <- trio_diff_unr[-c(1,2),]

# adjust p-values for intercept
trio_diff_unr <- trio_diff_unr %>%
  filter(lhs=="i1") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# take tot estimates from full sample trio PGS model
trio_tot_unr <- standardizedSolution(fit_triomodel_unrelated, ci=TRUE, level=0.95) %>%
  filter(lhs=="i2"|lhs=="s2",
         op == "~") %>%
  mutate(model = "Total problems")

trio_tot_unr <- trio_tot_unr[-c(1,2),]

# adjust p-values for intercept
trio_tot_unr <- trio_tot_unr %>%
  filter(lhs=="i2") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# merge diff and tot, create effect variable 
# distinguishing direct and indirect effects
trio_unrelated <- trio_diff_unr %>%
  full_join(trio_tot_unr) %>%
  mutate(Effect = case_when(str_detect(rhs, "_child") ~ "Direct\n (cPGS)",
                            !str_detect(rhs, "_child") &
                              model == "Total problems" |
                              model == "Differentiation" ~ "Indirect\n (mPGS + fPGS)"),
         Effect = fct_relevel(Effect,"Direct\n (cPGS)","Indirect\n (mPGS + fPGS)"),
         model = as.factor(model),
         pgs = as.factor(rhs))

levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="adhd_child"] <- "ADHD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="alc_child"] <- "Alcohol"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="an_child"] <- "Anorexia"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="anx_child"] <- "Anxiety"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="asd_child"] <- "Autism"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="bip_child"] <- "Bipolar"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="scz_child"] <- "SCZ"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="mdd_child"] <- "MDD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ocd_child"] <- "OCD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ptsd_child"] <- "PTSD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ts_child"] <- "Tourette's"

levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="adhd_mother"] <- "ADHD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="alc_mother"] <- "Alcohol"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="an_mother"] <- "Anorexia"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="anx_mother"] <- "Anxiety"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="asd_mother"] <- "Autism"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="bip_mother"] <- "Bipolar"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="scz_mother"] <- "SCZ"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="mdd_mother"] <- "MDD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ocd_mother"] <- "OCD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ptsd_mother"] <- "PTSD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ts_mother"] <- "Tourette's"

levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="adhd_father"] <- "ADHD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="alc_father"] <- "Alcohol"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="an_father"] <- "Anorexia"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="anx_father"] <- "Anxiety"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="asd_father"] <- "Autism"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="bip_father"] <- "Bipolar"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="scz_father"] <- "SCZ"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="mdd_father"] <- "MDD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ocd_father"] <- "OCD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ptsd_father"] <- "PTSD"
levels(trio_unrelated$pgs)[levels(trio_unrelated$pgs)=="ts_father"] <- "Tourette's"

# save out unrelated sample results
save(trio_unrelated,file="./output/trio_PGS_effects_unrelated_ld.RData")

# plot unadjusted and adjusted PGS associations
load(file="./output/trio_PGS_effects.RData")
load(file="./output/unadjusted_PGS_ests.RData")

# rename before merging unadj and adj
unadj_ests <- ests %>%
  rename(fdr_pval = FDR_pval) %>%
  mutate(model = "Child-only")

adj_trio <- trio_both %>%
  filter(Effect=="Direct\n (cPGS)") %>%
  select(-Effect) %>%
  rename(outcome = model) %>%
  mutate(model = "Trio-adjusted")

# merge
all_ests <- adj_trio %>%
  full_join(unadj_ests)

# fix order of factors
all_ests <- all_ests %>%
  mutate(model=fct_relevel(model,"Child-only","Trio-adjusted"),
         outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anxiety","MDD","PTSD","Autism","ADHD","Alcohol",
                         "Bipolar","SCZ","Tourette's","OCD","Anorexia"))

# create dot plot unadjusted + adjusted PGS
p_all<-ggplot(all_ests, aes(x=est.std, y=pgs, xmin=ci.lower, xmax=ci.upper, colour=outcome, shape=outcome)) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + theme_light(base_size=14) +
  geom_errorbar(size = 1.3, alpha=0.3, width = 0, position=position_dodge(0.62),show.legend = FALSE) +
  geom_point(size=3.8, position=position_dodge(0.62)) +
  facet_grid(cols = vars(model)) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  theme(axis.text = element_text(size=14,colour="black"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=18,colour="black"),
        text=element_text(size = 14),
        axis.line = element_line(colour="grey50",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=16,colour="black"),
        legend.position="top") + 
  xlab(label=expression(beta)) + 
  coord_cartesian(xlim = c(-0.075,0.125))

p_all

# plot unadjusted and adjusted PGS associations, both 
# estimated in trio sample to avoid selection effects
load(file="./output/unadjusted_PGS_ests_trio_sample.RData")

# rename before merging unadj and adj
unadj_ests_trio <- unadj_ests_trio %>%
  rename(fdr_pval = FDR_pval) %>%
  mutate(model = "Child-only")

adj_trio <- trio_both %>%
  filter(Effect=="Direct\n (cPGS)") %>%
  select(-Effect) %>%
  rename(outcome = model) %>%
  mutate(model = "Trio-adjusted")

# merge
all_ests <- adj_trio %>%
  full_join(unadj_ests_trio)

# fix order of factors
all_ests <- all_ests %>%
  mutate(model=fct_relevel(model,"Child-only","Trio-adjusted"),
         outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anxiety","MDD","PTSD","Autism","ADHD","Alcohol",
                         "Bipolar","SCZ","Tourette's","OCD","Anorexia")) %>%
  arrange(pgs,model,outcome)

all_ests_child <- all_ests %>%
  filter(model == "Child-only") %>%
  rename(x_start = est.std)

all_ests_trio <- all_ests %>%
  filter(model == "Trio-adjusted") %>%
  rename(x_end = est.std)

# Merge them back together based on 'pgs' and 'outcome'
all_ests_line <- all_ests_child %>%
  inner_join(all_ests_trio, by = c("pgs", "outcome"))

# Create dodge factor
all_ests_line$dodge_factor <- ifelse(all_ests_line$outcome == "Differentiation", -0.15, 0.15)

# Make sure 'pgs' is a factor
all_ests_line$pgs <- as.factor(all_ests_line$pgs)

# Create yend
all_ests_line$yend <- as.numeric(all_ests_line$pgs) + all_ests_line$dodge_factor

# cleveland dot plot child-only PGS and trio-adjusted direct effects
p_clev <- ggplot(all_ests_line, aes(colour = outcome, group = interaction(outcome, pgs))) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + 
  geom_segment(data = all_ests_line, aes(x = x_start, xend = x_end, y=pgs, yend = yend), position = position_dodge(0.6), colour="black") +
  geom_point(data = all_ests, aes(x = est.std, y=pgs, colour=outcome, shape = model), fill="white",position = position_dodge(0.6), size = 3.2) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  scale_shape_manual(values = c("Child-only" = 21, "Trio-adjusted" = 24)) +
  guides(colour="none") +
  theme_light(base_size=14) +
  theme(axis.text = element_text(size=14,colour="black"),
        axis.text.x = element_text(size=13,colour="black"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=15,colour="black"),
        text=element_text(size = 14),
        axis.line = element_line(colour="grey50",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=13,colour="black"),
        legend.position="top") + 
  xlab(label=expression(paste(beta," direct (cPGS)"))) + 
  coord_cartesian(xlim = c(-0.055,0.105))

# create plot for indirect genetic effects
trio_indirect <- trio_both %>%
  filter(Effect=="Indirect\n (mPGS + fPGS)") %>%
  select(-Effect) %>%
  rename(outcome = model) %>%
  mutate(parent = ifelse(str_detect(rhs,"_mother"),"Mother", "Father"))

trio_indirect_m <- trio_indirect %>%
  filter(parent=="Mother") %>%
  mutate(outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anxiety","MDD","PTSD","Autism","ADHD","Alcohol",
                         "Bipolar","SCZ","Tourette's","OCD","Anorexia")) %>%
  arrange(pgs,outcome)

trio_indirect_f <- trio_indirect %>%
  filter(parent=="Father") %>%
  mutate(outcome=fct_relevel(outcome,"Differentiation","Total problems"),
         pgs=fct_relevel(pgs,"Anxiety","MDD","PTSD","Autism","ADHD","Alcohol",
                         "Bipolar","SCZ","Tourette's","OCD","Anorexia")) %>%
  arrange(pgs,outcome)

p_ind_m <- ggplot(trio_indirect_m, aes(x = est.std, y = pgs, xmin=ci.lower, xmax=ci.upper, colour = outcome, group = interaction(outcome, pgs))) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + 
  geom_errorbar(size = 1.2, alpha=0.3, width = 0, position=position_dodge(0.6),show.legend = FALSE) +
  geom_point(position = position_dodge(0.6), size = 3.5) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  theme_light(base_size=14) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=13,colour="black"),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=15,colour="black"),
        text=element_text(size = 14),
        axis.line = element_line(colour="grey50",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=13,colour="black"),
        legend.position="top") + 
  scale_x_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05)) +
  xlab(label=expression(paste(beta," indirect (mPGS)"))) + 
  coord_cartesian(xlim = c(-0.053,0.053))

p_ind_f <- ggplot(trio_indirect_f, aes(x = est.std, y = pgs, xmin=ci.lower, xmax=ci.upper, colour = outcome, group = interaction(outcome, pgs))) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + 
  geom_errorbar(size = 1.2, alpha=0.3, width = 0, position=position_dodge(0.6),show.legend = FALSE) +
  geom_point(position = position_dodge(0.6), size = 3.5,show.legend = FALSE) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  theme_light(base_size=14) +
  theme(axis.text = element_text(size=14,colour="black"),
        axis.text.x = element_text(size=13,colour="black"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        axis.title.x = element_text(size=15,colour="black"),
        text=element_text(size = 14),
        axis.line = element_line(colour="grey50",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line(),
        legend.title = element_blank(),
        legend.position="top") + 
  scale_x_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05)) +
  xlab(label=expression(paste(beta," indirect (fPGS)"))) + 
  coord_cartesian(xlim = c(-0.053,0.053))

patch <- p_clev + p_ind_m + p_ind_f

patch6 <- patch + plot_annotation(tag_levels="A") &
  theme(plot.tag = element_text(size = 14))

# save out
tiff("figures/Figure_6.tiff", res = 800, compression = "lzw", unit = "in",
     height = 8, width = 11)

patch6

dev.off()



# plot results ----

# create dot plot unadjusted PGS
p1<-ggplot(ests, aes(x=est.std, y=pgs, xmin=ci.lower, xmax=ci.upper, colour=outcome, shape=outcome)) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + theme_light(base_size=14) +
  geom_errorbar(size = 1.3, alpha=0.3, width = 0, position=position_dodge(0.6),show.legend = FALSE) +
  geom_point(size=3.8, position=position_dodge(0.6)) +
  scale_y_discrete(limits=rev) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  theme(axis.text = element_text(size=14,colour="black"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=18,colour="black"),
        text=element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white"),
        legend.title = element_blank(),
        legend.text = element_text(size=14,colour="black"),
        legend.position="top",
        plot.margin=unit(c(0,1.6,0,0), "cm")) + 
  xlab(label=expression(beta)) + 
  coord_cartesian(xlim = c(-0.05,0.117))

trio_sum <- trio_both %>%
  mutate(bp_est = est.std^2) %>%
  group_by(Effect, model) %>%
  summarise(
    bp_est = sum(bp_est),
    .groups = "drop"
  ) %>%
  as.data.frame()

# create bar plot trio PGS
bp <- ggplot(trio_sum, mapping=aes(x=Effect, y=bp_est, fill=model)) +
  geom_hline(yintercept=0,size=0) + theme_void() +
  geom_bar(width = 0.65, stat="identity", position=position_dodge(0.66),show.legend = TRUE) +
  scale_fill_manual(values= c("#0072B2", "#D55E00")) +
  coord_cartesian(ylim=c(0,0.015)) +
  ylab("Variance explained") + 
  theme(axis.title.y=element_text(angle=90, size=15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13.5),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.spacing = unit(2, "cm"),
        plot.margin=unit(c(0,0,0.5,0), "cm"))

patch <- p1 | (guide_area() / bp / plot_spacer()) + plot_layout(guides="collect",
                                                                heights = c(0.5, 1, 0.2))
patch + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14),
        legend.direction = "horizontal")

# create tables ----

# create table direct genetic effects on intercept
tbl_int_trio_direct <- trio_both %>%
  filter(Effect=="Direct\n (cPGS)") %>%
  mutate(across(where(is.numeric), round, digits=2)) %>%
  select(model,pgs,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(pgs, ends_with(".d"), ends_with(".t"))

rows_to_replace_col5 <- which(tbl_int_trio_direct[[5]] == 0)
rows_to_replace_col9 <- which(tbl_int_trio_direct[[9]] == 0)

(ft_tbl_int_trio_direct <- tbl_int_trio_direct %>% 
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Differentiation","Total problems"), colwidths = c(1,4,4)) %>%
    compose(i = c(2,2), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    compose(i = rows_to_replace_col5, j = 5, part = "body",value = as_paragraph("<0.01")) %>%
    compose(i = rows_to_replace_col9, j = 9, part = "body",value = as_paragraph("<0.01")) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 1, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_trio_direct, path = "./tables/Table_S11_trio_direct_effects.docx")

# create table indirect genetic effects on intercept
tbl_int_trio_indirect <- trio_both %>%
  filter(Effect=="Indirect\n (mPGS + fPGS)") %>%
  mutate(across(where(is.numeric), round, digits=2),
         parent = ifelse(str_detect(rhs,"mother"),"Mother","Father")) %>%
  select(model,pgs,parent,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(parent, pgs, ends_with(".d"), ends_with(".t")) %>%
  mutate(parent = as.factor(parent)) %>%
  mutate(parent = fct_relevel(parent,"Mother","Father")) %>%
  arrange(parent)

(ft_tbl_int_trio_indirect <- tbl_int_trio_indirect %>% 
    flextable() %>% 
    set_header_labels(values=list(parent="PARENT",pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","","Differentiation","Total problems"), colwidths = c(1,1,4,4)) %>%
    compose(i = c(2,2), j = c(6,10), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    merge_v(j = ~ parent) %>%
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 2, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_trio_indirect, path = "./tables/Table_S14_trio_indirect_effects.docx")

# create table direct genetic effects (unrelated sensitivity)
tbl_unr_trio_direct <- trio_unrelated %>%
  filter(Effect=="Direct\n (cPGS)") %>%
  mutate(across(where(is.numeric), round, digits=2)) %>%
  select(model,pgs,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(pgs, ends_with(".d"), ends_with(".t"))

rows_to_replace_col5 <- which(tbl_unr_trio_direct[[5]] == 0)
rows_to_replace_col9 <- which(tbl_unr_trio_direct[[9]] == 0)

(ft_tbl_unr_trio_direct <- tbl_unr_trio_direct %>% 
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Differentiation","Total problems"), colwidths = c(1,4,4)) %>%
    compose(i = c(2,2), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    compose(i = rows_to_replace_col5, j = 5, part = "body",value = as_paragraph("<0.01")) %>%
    compose(i = rows_to_replace_col9, j = 9, part = "body",value = as_paragraph("<0.01")) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 1, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_unr_trio_direct, path = "./tables/Table_S15_trio_direct_unrelated.docx")

# create table indirect genetic effects (unrelated sensitivity)
tbl_unr_trio_indirect <- trio_unrelated %>%
  filter(Effect=="Indirect\n (mPGS + fPGS)") %>%
  mutate(across(where(is.numeric), round, digits=2),
         parent = ifelse(str_detect(rhs,"mother"),"Mother","Father")) %>%
  select(model,pgs,parent,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(parent, pgs, ends_with(".d"), ends_with(".t")) %>%
  mutate(parent = as.factor(parent)) %>%
  mutate(parent = fct_relevel(parent,"Mother","Father")) %>%
  arrange(parent)

(ft_tbl_unr_trio_indirect <- tbl_unr_trio_indirect %>% 
    flextable() %>% 
    set_header_labels(values=list(parent="PARENT",pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","","Differentiation","Total problems"), colwidths = c(1,1,4,4)) %>%
    compose(i = c(2,2), j = c(6,10), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    merge_v(j = ~ parent) %>%
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 2, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_unr_trio_indirect, path = "./tables/Table_S16_trio_indirect_unrelated.docx")

# create slope PGS plot

# merge diff and tot, create effect variable 
# distinguishing direct and indirect effects
trio_both2 <- trio_diff_slope %>%
  full_join(trio_tot_slope) %>%
  mutate(Effect = case_when(str_detect(rhs, "_child") ~ "Direct\n (cPGS)",
                            !str_detect(rhs, "_child") &
                              model == "Total problems" |
                              model == "Differentiation" ~ "Indirect\n (mPGS + fPGS)"),
         Effect = fct_relevel(Effect,"Direct\n (cPGS)","Indirect\n (mPGS + fPGS)"),
         model = as.factor(model))

#save out trio results slope
save(trio_both2, file="./output/trio_both_slope.RData")
load(file="./output/trio_both_slope.RData")

# load unadjusted PGS results
load(file="./output/unadjusted_PGS_ests_slope.RData")

# create dot plot unadjusted PGS for slope
(p2<-ggplot(ests2, aes(x=est.std, y=pgs, xmin=ci.lower, xmax=ci.upper, colour=outcome, shape=outcome)) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + theme_light(base_size=14) +
  geom_errorbar(size = 1.3, alpha=0.3, width = 0, position=position_dodge(0.4),show.legend = FALSE) +
  geom_point(size=3.8, position=position_dodge(0.4)) +
  scale_y_discrete(limits=rev) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  theme(axis.text = element_text(size=14,colour="black"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=18,colour="black"),
        text=element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour="black",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white"),
        legend.title = element_blank(),
        legend.text = element_text(size=14,colour="black"),
        legend.position="top",
        plot.margin=unit(c(0,1.6,0,0), "cm")) + 
  xlab(label=expression(beta)) + 
  coord_cartesian(xlim = c(-0.05,0.105)))

# rename before merging unadj and adj
unadj_ests2 <- ests2 %>%
  rename(fdr_pval = FDR_pval) %>%
  mutate(model = "Child PGS unadjusted")

adj_trio2 <- trio_both2 %>%
  filter(Effect=="Direct\n (cPGS)") %>%
  select(-Effect) %>%
  rename(outcome = model) %>%
  mutate(pgs = as.factor(rhs),
         model = "Child PGS trio-adjusted")

levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="adhd_child"] <- "ADHD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="alc_child"] <- "Alcohol"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="an_child"] <- "Anorexia"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="anx_child"] <- "Anxiety"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="asd_child"] <- "Autism"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="bip_child"] <- "Bipolar"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="scz_child"] <- "SCZ"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="mdd_child"] <- "MDD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="ocd_child"] <- "OCD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="ptsd_child"] <- "PTSD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="ts_child"] <- "Tourette's"

# merge
all_ests2 <- adj_trio2 %>%
  full_join(unadj_ests2)

# fix order of factors
all_ests2 <- all_ests2 %>%
  mutate(model=fct_relevel(model,"Child PGS unadjusted","Child PGS trio-adjusted"),
         pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                         "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

# create dot plot unadjusted + adjusted PGS for slope
p_all2<-ggplot(all_ests2, aes(x=est.std, y=pgs, xmin=ci.lower, xmax=ci.upper, colour=outcome, shape=outcome)) +
  geom_vline(aes(xintercept=0), size=0.9,colour = "grey60", linetype = 3) + theme_light(base_size=14) +
  geom_errorbar(size = 1.3, alpha=0.3, width = 0, position=position_dodge(0.62),show.legend = FALSE) +
  geom_point(size=3.8, position=position_dodge(0.62)) +
  facet_grid(cols = vars(model)) +
  scale_y_discrete(limits=rev) +
  scale_colour_manual(values= c("#0072B2", "#D55E00")) +
  theme(axis.text = element_text(size=14,colour="black"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=18,colour="black"),
        text=element_text(size = 14),
        axis.line = element_line(colour="grey50",size=0),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=16,colour="black"),
        legend.position="top") + 
  xlab(label=expression(beta)) + 
  coord_cartesian(xlim = c(-0.075,0.125))

p_all2

# create tables slope ----

levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="adhd_child"] <- "ADHD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="alc_child"] <- "Alcohol"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="an_child"] <- "Anorexia"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="anx_child"] <- "Anxiety"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="asd_child"] <- "Autism"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="bip_child"] <- "Bipolar"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="scz_child"] <- "SCZ"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="mdd_child"] <- "MDD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="ocd_child"] <- "OCD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="ptsd_child"] <- "PTSD"
levels(adj_trio2$pgs)[levels(adj_trio2$pgs)=="ts_child"] <- "Tourette's"

# create table direct genetic effects on slope
tbl_slope_trio_direct <- all_ests2 %>%
  filter(model=="Child PGS trio-adjusted") %>%
  mutate(across(where(is.numeric), round, digits=2)) %>%
  select(outcome,pgs,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(outcome = if_else(outcome == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = outcome, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(pgs, ends_with(".d"), ends_with(".t"))

rows_to_replace_col5 <- which(tbl_slope_trio_direct[[5]] == 0)
rows_to_replace_col9 <- which(tbl_slope_trio_direct[[9]] == 0)

(ft_tbl_slope_trio_direct <- tbl_slope_trio_direct %>% 
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Differentiation","Total problems"), colwidths = c(1,4,4)) %>%
    compose(i = c(2,2), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    compose(i = rows_to_replace_col5, j = 5, part = "body",value = as_paragraph("<0.01")) %>%
    compose(i = rows_to_replace_col9, j = 9, part = "body",value = as_paragraph("<0.01")) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 1, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_slope_trio_direct, path = "./tables/Table_S18_trio_direct_effects_slope.docx")

# create table indirect genetic effects on slope
trio_both2 <- trio_both2 %>%
  mutate(pgs = as.factor(rhs))

#rename pgs
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="adhd_child"] <- "ADHD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="alc_child"] <- "Alcohol"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="an_child"] <- "Anorexia"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="anx_child"] <- "Anxiety"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="asd_child"] <- "Autism"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="bip_child"] <- "Bipolar"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="scz_child"] <- "SCZ"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="mdd_child"] <- "MDD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ocd_child"] <- "OCD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ptsd_child"] <- "PTSD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ts_child"] <- "Tourette's"

levels(trio_both2$pgs)[levels(trio_both2$pgs)=="adhd_mother"] <- "ADHD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="alc_mother"] <- "Alcohol"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="an_mother"] <- "Anorexia"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="anx_mother"] <- "Anxiety"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="asd_mother"] <- "Autism"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="bip_mother"] <- "Bipolar"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="scz_mother"] <- "SCZ"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="mdd_mother"] <- "MDD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ocd_mother"] <- "OCD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ptsd_mother"] <- "PTSD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ts_mother"] <- "Tourette's"

levels(trio_both2$pgs)[levels(trio_both2$pgs)=="adhd_father"] <- "ADHD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="alc_father"] <- "Alcohol"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="an_father"] <- "Anorexia"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="anx_father"] <- "Anxiety"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="asd_father"] <- "Autism"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="bip_father"] <- "Bipolar"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="scz_father"] <- "SCZ"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="mdd_father"] <- "MDD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ocd_father"] <- "OCD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ptsd_father"] <- "PTSD"
levels(trio_both2$pgs)[levels(trio_both2$pgs)=="ts_father"] <- "Tourette's"

tbl_slope_trio_indirect <- trio_both2 %>%
  filter(Effect=="Indirect\n (mPGS + fPGS)") %>%
  mutate(pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                         "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety")) %>%
  mutate(across(where(is.numeric), round, digits=2),
         parent = ifelse(str_detect(rhs,"mother"),"Mother","Father")) %>%
  select(model,pgs,parent,est.std,ci.lower,ci.upper,fdr_pval) %>%
  mutate(model = if_else(model == "Differentiation", "d", "t")) %>%
  pivot_wider(names_from = model, 
              values_from = c(est.std, ci.lower, ci.upper, fdr_pval),
              names_sep = ".") %>%
  select(parent, pgs, ends_with(".d"), ends_with(".t")) %>%
  mutate(parent = as.factor(parent)) %>%
  mutate(parent = fct_relevel(parent,"Mother","Father")) %>%
  arrange(parent)

(ft_tbl_slope_trio_indirect <- tbl_slope_trio_indirect %>% 
    flextable() %>% 
    set_header_labels(values=list(parent="PARENT",pgs="PGS",est.std.d="EST",ci.lower.d="LCI",
                                  ci.upper.d="UCI",fdr_pval.d="",est.std.t="EST",
                                  ci.lower.t="LCI",ci.upper.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","","Differentiation","Total problems"), colwidths = c(1,1,4,4)) %>%
    compose(i = c(2,2), j = c(6,10), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% 
    theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    merge_v(j = ~ parent) %>%
    align(align = "right", part="footer") %>% 
    align(align = "center", part="header") %>%
    align(j = 2, align = "left", part = "header") %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_slope_trio_indirect, path = "./tables/Table_S19_trio_indirect_effects_slope.docx")

