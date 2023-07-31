#07.2_run_mediation_LGM_dx.R

## This script runs mediation LGMs with diagnostic outcomes in Mplus, 
## using the MplusAutomation package. 

library(tidyverse)
library(MplusAutomation)

# To install MplusAutomation:

# for(each in list.files("//ess01/P471/data/durable/common/software/mA_deps_repo/4.0")){
#   install.packages(paste0("//ess01/P471/data/durable/common/software/mA_deps_repo/4.0/",each),
#                    repos=NULL,
#                    type = "binary")
# }

# read in processed data from 01_data_preparation.R
load("./data/processed_data.RData")

# Prepare for Mplus
dat <- fulldata %>% 
  select(ind_id, m_id, sex, BARN_NR, 
         age1, age2, age3, 
         diff1, diff2, diff3, tot1, tot2, tot3,
         inat_8yr, hyp_8yr, cd_8yr, odd_8yr, dep_8yr, anx_8yr,
         any_dx_dep, any_dx_anx, any_dx_con, any_dx_adhd, 
         adhd_child, asd_child, scz_child, bip_child, mdd_child, anx_child, 
         alc_child, ptsd_child, ocd_child, an_child, ts_child) %>% 
  rename_with(~str_remove(.,"any_")) %>% 
  mutate(ind_id=factor(ind_id),
         m_id=factor(m_id),
         sex=as.factor(sex),
         BARN_NR= as.factor(BARN_NR),
         dx_dep=as.factor(dx_dep),
         dx_anx=as.factor(dx_anx),
         dx_con=as.factor(dx_con),
         dx_adhd=as.factor(dx_adhd),
         across(where(is.numeric),scale),
         across(where(is.numeric),as.numeric),
         sex=as.numeric(sex),
         dx_dep=as.numeric(dx_dep)-1,
         dx_anx=as.numeric(dx_anx)-1,
         dx_con=as.numeric(dx_con)-1,
         dx_adhd=as.numeric(dx_adhd)-1) %>% 
  as.data.frame()
###
# Rename x AND Y-variable columns to make mplus coding easier - save a lookup
# table for re-identifying these variables later
mplusdat<- dat
colnames(mplusdat) <- c(names(mplusdat %>% select(ind_id:tot3)),
                        paste0("y",seq(1,6,1)),
                        paste0("yd",seq(1,4,1)),
                        paste0("x",seq(1,ncol(mplusdat)-length(names(mplusdat %>% select(ind_id:dx_adhd))),1)))

lkp_names <- tibble(oldnames = names(dat),
                    newnames = names(mplusdat))

save(lkp_names, file= "./data/mplus_names_lkp.RData")
load(file= "./data/mplus_names_lkp.RData")

#use MplusAutomation to make the data in an Mplus friendly format
prepareMplusData(mplusdat, "//ess01/p471/data/durable/projects/differentiation_genomics/scripts/mplus/data/data_for_mlevel.dat")

#run mediation models
filepath1 <- "./scripts/mplus/scripts/mediation_lgm"

runModels(filepath1, recursive=F, showOutput=T, replaceOutfile="modifiedDate", Mplus_command="C:/Program Files/Mplus/Mplus")

# Move the results files to the output file and clean up the scripts folder
file.copy(from=paste0(filepath1,"/",list.files(filepath1)[str_detect(list.files(filepath1),".inp",negate=T)]),
          to="./scripts/mplus/output/mediation_lgm",
          overwrite = TRUE, recursive = F,
          copy.mode = TRUE)

junk <- dir(path=filepath1, pattern=".out|.dat") 
file.remove(paste0(filepath1,"/",junk))

# Read in output
mplusOutput <- readModels("./scripts/mplus/output/mediation_lgm", recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# Read in model indirect results
tmp_res <- readLines("./scripts/mplus/output/mediation_lgm/allprs_lgm_both_mediation_dx.out")
ind_res <- tmp_res[(grep("^TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS$",tmp_res)+5):
                     (grep("CREDIBILITY INTERVALS OF MODEL RESULTS",tmp_res)-2)] %>% 
  as.data.frame() %>% 
  `colnames<-`("tmp")
ind_res_names <- ind_res %>% 
  filter(str_detect(tmp, "Effects from")) 
indirect_ests <- ind_res %>% 
  filter(str_detect(tmp,"X"),
         !str_detect(tmp,"Effects")) %>%  
  mutate(tmp = gsub("\\s+", " ", str_trim(tmp))) %>% 
  separate(tmp, into= c("pgs",c("est","post_sd","onet_pval","lci","uci")),sep=" ") %>% 
  mutate(model = rep(c("diff","tot"),times=nrow(ind_res_names)),
         yvar = str_remove_all(rep(ind_res_names$tmp, each=2), "Effects from "),
         pgs= tolower(pgs)) %>% 
  separate(yvar, into =c("null", "yvar"), sep=" to ") %>% 
  mutate(yvar = tolower(yvar)) %>% 
  select(-null) %>% 
  left_join(lkp_names %>% 
              rename("pgs"=newnames, "pgs_real"=oldnames)) %>% 
  left_join(lkp_names %>% 
              rename("yvar"=newnames, "outcome"=oldnames)) %>% 
  select(pgs_real,outcome,model,est:uci)

# extract relevant parameters and format
direct_ests <- mplusOutput$parameters$unstandardized %>% 
  filter(str_detect(paramHeader,"^YD"),
         str_detect(param,"^X"))  %>%
  mutate(param = str_replace_all(param, fixed("X"), "x"),
         paramHeader = str_replace_all(paramHeader, fixed("YD"), "yd"),
         paramHeader = str_remove_all(paramHeader, ".ON")) %>%
  select(param, paramHeader, est, post_sd=posterior_sd, onet_pval=pval, 
         lci=lower_2.5ci, uci=upper_2.5ci) %>% 
  left_join(lkp_names %>% 
              rename("param"=newnames, "pgs_real"=oldnames)) %>% 
  left_join(lkp_names %>% 
              rename("paramHeader"=newnames, "outcome"=oldnames)) %>% 
  select(pgs_real,outcome,est:uci)
         
# save out/load
save(indirect_ests,file="./output/indirect_ests.RData")
load(file="./output/indirect_ests.RData")
save(direct_ests,file="./output/direct_ests.RData")
load(file="./output/direct_ests.RData")

###############################################################
##### plot mediation LGM with PGS on each dx (diff + tot) #####
###############################################################

# adjust p-values for direct and indirect
dx_both_direct <- direct_ests %>%
  mutate(fdr_pval = p.adjust(onet_pval, method="fdr")) %>%
  select(-onet_pval) %>%
  mutate(effect="Unmediated")

dx_both_indirect <- indirect_ests %>%
  mutate(fdr_pval = p.adjust(onet_pval, method="fdr")) %>%
  select(-onet_pval) %>%
  rename(effect=model) %>%
  mutate(effect = case_when(effect == "diff" ~ "Differentiation",
                            effect == "tot" ~ "Total problems"))

# format output
dx_ests <- dx_both_indirect %>%
  mutate(est=as.numeric(est),
         post_sd=as.numeric(post_sd),
         lci=as.numeric(lci),
         uci=as.numeric(uci)) %>%
  full_join(dx_both_direct) %>%
  mutate(pgs = case_when(str_detect(pgs_real, "an_") ~ "Anorexia",
                         str_detect(pgs_real, "ocd_") ~ "OCD",
                         str_detect(pgs_real, "ts_") ~ "Tourette's",
                         str_detect(pgs_real, "scz_") ~ "SCZ",
                         str_detect(pgs_real, "bip_") ~ "Bipolar",
                         str_detect(pgs_real, "alc_") ~ "Alcohol",
                         str_detect(pgs_real, "adhd_") ~ "ADHD",
                         str_detect(pgs_real, "asd_") ~ "Autism",
                         str_detect(pgs_real, "ptsd_") ~ "PTSD",
                         str_detect(pgs_real, "mdd_") ~ "MDD",
                         str_detect(pgs_real, "anx_") ~ "Anxiety"),        
         outcome = case_when(str_detect(outcome, "_anx") ~ "Anxiety disorder",
                             str_detect(outcome, "_dep") ~ "Depressive disorder",
                             str_detect(outcome, "_adhd") ~ "ADHD",
                             str_detect(outcome, "_con") ~ "Disruptive disorder")) %>%
  select(pgs,outcome,effect,est,post_sd,lci,uci,fdr_pval)

# create fdr variable and reorder factors
dx_ests <- dx_ests %>%
  mutate(fdr = case_when(fdr_pval < .05 ~ "sig",
                         fdr_pval > .05 ~ "non-sig"),
         outcome = fct_relevel(outcome,"Anxiety disorder","Depressive disorder",
                               "ADHD","Disruptive disorder"),
         pgs = fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                           "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"),
         effect = fct_relevel(effect,"Total problems","Differentiation","Unmediated"))

## create tables dx

# direct and mediated PGS effects on diagnostic outcomes (via diff+tot)
tbl_mediation_dx <- dx_ests %>%
  select(pgs,outcome,effect,est,lci,uci,fdr_pval) %>%
  mutate(effect = case_when(effect == "Differentiation" ~ "d",
                            effect == "Total problems" ~ "t",
                            effect == "Unmediated" ~ "u")) %>%
  pivot_wider(names_from = effect, 
              values_from = c(est,lci,uci,fdr_pval),
              names_sep = ".") %>%
  select(pgs, outcome, ends_with(".u"), ends_with(".d"), ends_with(".t")) %>%
  mutate(across(where(is.numeric), round, digits=2))

## Table S16: adhd dx table
(ft_tbl_mediation_adhd_dx <- tbl_mediation_dx %>% 
    filter(outcome=="ADHD") %>%
    select(-outcome) %>%
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.u="EST",lci.u="LCI",
                                  uci.u="UCI",fdr_pval.u="",est.d="EST",lci.d="LCI",
                                  uci.d="UCI",fdr_pval.d="",est.t="EST",lci.t="LCI",
                                  uci.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Unmediated","Mediated via differentiation","Mediated via total problems"), colwidths = c(1,4,4,4)) %>%
    compose(i = c(2,2,2), j = c(5,9,13), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "center", part="all"))

save_as_docx(ft_tbl_mediation_adhd_dx, path = "./tables/Table_S16_mediation_effects_adhd_dx.docx")

## Table S17: disruptive disorder dx table
(ft_tbl_mediation_dbd_dx <- tbl_mediation_dx %>% 
    filter(outcome=="Disruptive disorder") %>%
    select(-outcome) %>%
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.u="EST",lci.u="LCI",
                                  uci.u="UCI",fdr_pval.u="",est.d="EST",lci.d="LCI",
                                  uci.d="UCI",fdr_pval.d="",est.t="EST",lci.t="LCI",
                                  uci.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Unmediated","Mediated via differentiation","Mediated via total problems"), colwidths = c(1,4,4,4)) %>%
    compose(i = c(2,2,2), j = c(5,9,13), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "center", part="all"))

save_as_docx(ft_tbl_mediation_dbd_dx, path = "./tables/Table_S17_mediation_effects_dbd_dx.docx")

## Table S18: depressive dx table
(ft_tbl_mediation_dep_dx <- tbl_mediation_dx %>% 
    filter(outcome=="Depressive disorder") %>%
    select(-outcome) %>%
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.u="EST",lci.u="LCI",
                                  uci.u="UCI",fdr_pval.u="",est.d="EST",lci.d="LCI",
                                  uci.d="UCI",fdr_pval.d="",est.t="EST",lci.t="LCI",
                                  uci.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Unmediated","Mediated via differentiation","Mediated via total problems"), colwidths = c(1,4,4,4)) %>%
    compose(i = c(2,2,2), j = c(5,9,13), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "center", part="all"))

save_as_docx(ft_tbl_mediation_dep_dx, path = "./tables/Table_S18_mediation_effects_dep_dx.docx")

## Table S19: anxiety disorder dx table
(ft_tbl_mediation_anx_dx <- tbl_mediation_dx %>% 
    filter(outcome=="Anxiety disorder") %>%
    select(-outcome) %>%
    flextable() %>% 
    set_header_labels(values=list(pgs="PGS",est.u="EST",lci.u="LCI",
                                  uci.u="UCI",fdr_pval.u="",est.d="EST",lci.d="LCI",
                                  uci.d="UCI",fdr_pval.d="",est.t="EST",lci.t="LCI",
                                  uci.t="UCI",fdr_pval.t="")) %>%
    add_header_row(values=list("","Unmediated","Mediated via differentiation","Mediated via total problems"), colwidths = c(1,4,4,4)) %>%
    compose(i = c(2,2,2), j = c(5,9,13), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Arial", part= "all") %>% theme_box() %>%
    border_outer(border=fp_border(color="gray80",width=2),part="all") %>%
    border_inner(border=fp_border(color='gray80',width=1),part="all") %>%
    hline(i=2,border=fp_border(color="gray50",width=3),part="header") %>%
    bg(bg="#F1F1F1",part="header") %>% 
    align(align = "center", part="all"))

save_as_docx(ft_tbl_mediation_anx_dx, path = "./tables/Table_S19_mediation_effects_anx_dx.docx")

# make sign positive and sum
dx_sum <- dx_ests %>%
  mutate(bp_est = est^2) %>%
  group_by(effect) %>%
  summarise(bp_est = sum(bp_est),
            .groups = "drop") %>% 
  as.data.frame()

dx_sum <- dx_sum %>% 
  mutate(perc = `bp_est` / sum(`bp_est`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
dx_sum$ymax <- cumsum(dx_sum$perc)

# compute the bottom of each rectangle
dx_sum$ymin <- c(0, head(dx_sum$ymax, n=-1))

# compute label position
dx_sum$labelPosition <- (dx_sum$ymax + dx_sum$ymin) / 2

# create donut chart
dx<-ggplot(dx_sum, aes(ymax=ymax, ymin=ymin, xmax=3.9, xmin=3, fill=effect)) +
  geom_rect(show.legend=F) +
  geom_text( x=5,y=c(0.013,0.053,0.55), aes(label=labels, color=effect), size= 4 ,show.legend=FALSE) +
  geom_text( x=-1.1,y=-0.5, aes(label="Diagnoses"),color="black", size = 5, show.legend=FALSE) +
  scale_colour_manual(values= c("#0072B2","#D55E00","#99CCCC")) +
  scale_fill_manual(values= c("#0072B2","#D55E00","#99CCCC")) +
  guides(fill=guide_legend()) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin=unit(c(0,0,0,0.6), "cm"))

dx

