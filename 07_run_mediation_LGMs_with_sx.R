#07_run_mediation_LGMs_with_sx.R

## This script runs the mediation latent growth models (LGMs) w/ PGS
## and symptom outcomes by sourcing '07.1_specify_mediation_LGMs_with_sx.R'

library(tidyverse)
library(lavaan)
library(ggsci)

# read in processed data from 01_data_preparation.R
load("./data/processed_data.RData")

# specify mediation LGMs for diff and tot
source("./scripts/07.1_specify_mediation_LGMs_with_pgs.R")

# run mediation LGM with PGS on sympt (differentiation)
fit_model1 <- sem(model1, 
                  missing = "ML", 
                  estimator = "MLR", 
                  data = fulldata, 
                  cluster = "m_id",
                  fixed.x = F,
                  se = "bootstrap",
                  bootstrap = 10000)

# provide output for the model
summary(fit_model1, fit.measures = TRUE, std = TRUE)

# save output
save(fit_model1, file= "./output/fit_model1.RData")
load("./output/fit_model1.RData")

# format output for visualisation
sympt_diffests <- standardizedSolution(fit_model1, ci = TRUE, level = 0.95) %>%
  filter(op ==":=" | op=="~" & lhs !="i" & lhs !="s" & rhs !="i" & rhs !="s") %>%
  mutate(effect = case_when(str_detect(label, "c_") ~ "Direct", 
                            str_detect(label, "ab_s") ~ "Mediated (slope)",
                            str_detect(label, "ab_i") ~ "Mediated",
                            str_detect(label, "total") ~ "Total")) %>%
  filter(effect == "Direct" | effect == "Mediated") %>%
  mutate(pgs = case_when(str_ends(label, "_an") ~ "Anorexia",
                         str_ends(label, "_ocd") ~ "OCD",
                         str_ends(label, "_ts") ~ "Tourette's",
                         str_ends(label, "_scz") ~ "SCZ",
                         str_ends(label, "_bip") ~ "Bipolar",
                         str_ends(label, "_alc") ~ "Alcohol",
                         str_ends(label, "_adhd") ~ "ADHD",
                         str_ends(label, "_asd") ~ "Autism",
                         str_ends(label, "_ptsd") ~ "PTSD",
                         str_ends(label, "_mdd") ~ "MDD",
                         str_ends(label, "_anx") ~ "Anxiety"),        
         outcome = case_when(str_detect(label, "_anx_") ~ "Anxiety",
                             str_detect(label, "_dep_") ~ "Depression",
                             str_detect(label, "_hyp_") ~ "Hyperactivity",
                             str_detect(label, "_inat_") ~ "Inattention",
                             str_detect(label, "_cd_") ~ "Conduct",
                             str_detect(label, "_odd_") ~ "Oppositional"),
         est = est.std,
         lci = ci.lower,
         uci = ci.upper) %>%
  select(pgs,outcome,effect,est,lci,uci,pvalue)

# adjust p-values for direct and indirect
sympt_diff_direct <- sympt_diffests %>%
  filter(effect=="Direct") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

sympt_diff_indirect <- sympt_diffests %>%
  filter(effect=="Mediated") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# merge direct + indirect, create fdr variable and reorder factors
sympt_diffests <- sympt_diff_direct %>%
  full_join(sympt_diff_indirect) %>%
  mutate(fdr = ifelse(fdr_pval < .05, "sig","non-sig"),
         outcome = fct_relevel(outcome,"Anxiety","Depression","Conduct",
                               "Hyperactivity","Inattention","Oppositional"),
         pgs = fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                           "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"),
         effect = fct_relevel(effect,"Mediated","Direct"))

# create colour scale
my_colours <- c(ggsci::pal_jco("default")(10),"#E18727FF")

# generate bar plot
p1_bar<- ggplot(sympt_diffests, mapping=aes(x=pgs, y=est, fill=pgs, colour=pgs, alpha=effect)) +
  geom_col(width=0.8,position="stack") + theme_light() +
  facet_wrap(facets = vars(outcome)) +
  scale_fill_manual(values=my_colours) +
  scale_colour_manual(values=my_colours) +
  scale_x_discrete(limits=rev) +
  scale_alpha_ordinal(range=c(0.1,1)) +
  scale_y_continuous(breaks=c(0,0.05), limits=c(-0.037,0.087)) +
  guides(alpha=guide_legend(reverse=T),fill=guide_legend(override.aes=list(shape=21))) +
  theme(axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=17),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.line = element_line(colour="grey80"),
        plot.margin = margin(r = 0.2),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) +
  ylab(label="Standardised beta") + 
  coord_flip()

p1_bar

# run mediation LGM with PGS on sympt (total problems)
fit_model2 <- sem(model2, 
                  missing = "ML", 
                  estimator = "MLR", 
                  data = fulldata, 
                  cluster = "m_id",
                  fixed.x = F,
                  se = "bootstrap",
                  bootstrap = 10000)

# provide output for the model
summary(fit_model2, fit.measures = TRUE, std = TRUE)

# save output
save(fit_model2, file= "./output/fit_model2.RData")
load("./output/fit_model2.RData")

# format output for visualisation
sympt_totests <- standardizedSolution(fit_model2, ci = TRUE, level = 0.95) %>%
  filter(op ==":=" | op=="~" & lhs !="i" & lhs !="s" & rhs !="i" & rhs !="s") %>%
  mutate(effect = case_when(str_detect(label, "c_") ~ "Direct", 
                            str_detect(label, "ab_s") ~ "Mediated (slope)",
                            str_detect(label, "ab_i") ~ "Mediated",
                            str_detect(label, "total") ~ "Total")) %>%
  filter(effect == "Direct" | effect == "Mediated") %>%
  mutate(pgs = case_when(str_ends(label, "_an") ~ "Anorexia",
                         str_ends(label, "_ocd") ~ "OCD",
                         str_ends(label, "_ts") ~ "Tourette's",
                         str_ends(label, "_scz") ~ "SCZ",
                         str_ends(label, "_bip") ~ "Bipolar",
                         str_ends(label, "_alc") ~ "Alcohol",
                         str_ends(label, "_adhd") ~ "ADHD",
                         str_ends(label, "_asd") ~ "Autism",
                         str_ends(label, "_ptsd") ~ "PTSD",
                         str_ends(label, "_mdd") ~ "MDD",
                         str_ends(label, "_anx") ~ "Anxiety"),        
         outcome = case_when(str_detect(label, "_anx_") ~ "Anxiety",
                             str_detect(label, "_dep_") ~ "Depression",
                             str_detect(label, "_hyp_") ~ "Hyperactivity",
                             str_detect(label, "_inat_") ~ "Inattention",
                             str_detect(label, "_cd_") ~ "Conduct",
                             str_detect(label, "_odd_") ~ "Oppositional"),
         est = est.std,
         lci = ci.lower,
         uci = ci.upper) %>%
  select(pgs,outcome,effect,est,lci,uci,pvalue)

# adjust p-values for direct and indirect
sympt_tot_direct <- sympt_totests %>%
  filter(effect=="Direct") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

sympt_tot_indirect <- sympt_totests %>%
  filter(effect=="Mediated") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# merge direct + indirect, create fdr variable and reorder factors
sympt_totests <- sympt_tot_direct %>%
  full_join(sympt_tot_indirect) %>%
  mutate(fdr = ifelse(fdr_pval < .05, "sig","non-sig"),
         outcome = fct_relevel(outcome,"Anxiety","Depression","Conduct",
                               "Hyperactivity","Inattention","Oppositional"),
         pgs = fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                           "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"),
         effect = fct_relevel(effect,"Mediated","Direct"))

# generate bar plot
p2_bar<- ggplot(sympt_totests, mapping=aes(x=pgs, y=est, fill=pgs, colour=pgs, alpha=effect)) +
  geom_col(width=0.8,position="stack") + theme_light() +
  facet_wrap(facets = vars(outcome)) +
  scale_fill_manual(values=my_colours) +
  scale_colour_manual(values=my_colours) +
  scale_x_discrete(limits=rev) +
  scale_alpha_ordinal(range=c(0.1,1)) +
  scale_y_continuous(breaks=c(0,0.05), limits=c(-0.037,0.087)) +
  guides(alpha=guide_legend(reverse=T),fill=guide_legend(override.aes=list(shape=21))) +
  theme(axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=17),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.line = element_line(colour="grey80"),
        plot.margin = margin(r = 0.2),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) +
  ylab(label="Standardised beta") + 
  coord_flip()

p2_bar

# run mediation LGM with PGS on sympt (diff + total problems)
fit_model_both_mediation <- sem(model_both_sympt, 
                                missing = "ML", 
                                estimator = "MLR", 
                                data = fulldata, 
                                cluster = "m_id",
                                fixed.x = F,
                                se = "bootstrap",
                                bootstrap = 10000)

# provide output for the model
summary(fit_model_both_mediation, fit.measures = TRUE, std = TRUE)

# save output
save(fit_model_both_mediation, file= "./output/fit_model_both_mediation.RData")
load("./output/fit_model_both_mediation.RData")

# format output for visualisation
sympt_both_ests <- standardizedSolution(fit_model_both_mediation, ci = TRUE, level = 0.95) %>%
  filter(op ==":=" | op=="~" & lhs !="i1" & lhs !="s1" & rhs !="i1" & 
           rhs !="s1" & lhs !="i2" & lhs !="s2" & rhs !="i2" & rhs !="s2") %>%
  mutate(effect = case_when(str_detect(label, "c_") ~ "Unmediated", 
                            str_detect(label, "ab_s1") ~ "Differentiation\n (mediated slope)",
                            str_detect(label, "ab_i1") ~ "Differentiation",
                            str_detect(label, "ab_s2") ~ "Total problems\n (mediated slope)",
                            str_detect(label, "ab_i2") ~ "Total problems",
                            str_detect(label, "total") ~ "Total")) %>%
  filter(effect=="Unmediated"|effect=="Total problems"|effect=="Differentiation") %>%
  mutate(pgs = case_when(str_ends(label, "_an") ~ "Anorexia",
                         str_ends(label, "_ocd") ~ "OCD",
                         str_ends(label, "_ts") ~ "Tourette's",
                         str_ends(label, "_scz") ~ "SCZ",
                         str_ends(label, "_bip") ~ "Bipolar",
                         str_ends(label, "_alc") ~ "Alcohol",
                         str_ends(label, "_adhd") ~ "ADHD",
                         str_ends(label, "_asd") ~ "Autism",
                         str_ends(label, "_ptsd") ~ "PTSD",
                         str_ends(label, "_mdd") ~ "MDD",
                         str_ends(label, "_anx") ~ "Anxiety"),        
         outcome = case_when(str_detect(label, "_anx_") ~ "Anxiety",
                             str_detect(label, "_dep_") ~ "Depression",
                             str_detect(label, "_hyp_") ~ "Hyperactivity",
                             str_detect(label, "_inat_") ~ "Inattention",
                             str_detect(label, "_cd_") ~ "Conduct",
                             str_detect(label, "_odd_") ~ "Oppositional"),
         est = est.std,
         lci = ci.lower,
         uci = ci.upper) %>%
  select(pgs,outcome,effect,est,lci,uci,pvalue)

# adjust p-values for direct and indirect
sympt_both_direct <- sympt_both_ests %>%
  filter(effect=="Unmediated") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

sympt_both_indirect <- sympt_both_ests %>%
  filter(effect=="Total problems"|effect=="Differentiation") %>%
  mutate(fdr_pval = p.adjust(pvalue, method="fdr"))

# merge direct + indirect, create fdr variable and reorder factors
sympt_both_ests <- sympt_both_direct %>%
  full_join(sympt_both_indirect) %>%
  mutate(fdr = ifelse(fdr_pval < .05, "sig","non-sig"),
         outcome = fct_relevel(outcome,"Anxiety","Depression","Conduct",
                               "Hyperactivity","Inattention","Oppositional"),
         pgs = fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                           "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"),
         effect = fct_relevel(effect,"Differentiation","Total problems","Unmediated"))

# create tables mediation results

# direct and mediated PGS effects (via diff + tot)
tbl_mediation <- sympt_both_ests %>%
  select(pgs,outcome,effect,est,lci,uci,fdr_pval) %>%
  mutate(effect = case_when(effect == "Differentiation" ~ "d",
                           effect == "Total problems" ~ "t",
                           effect == "Unmediated" ~ "u")) %>%
  pivot_wider(names_from = effect, 
              values_from = c(est,lci,uci,fdr_pval),
              names_sep = ".") %>%
  select(pgs, outcome, ends_with(".u"), ends_with(".d"), ends_with(".t")) %>%
  mutate(across(where(is.numeric), round, digits=2))

# Table S10 - inattention symptoms
(ft_tbl_mediation_inat <- tbl_mediation %>% 
    filter(outcome=="Inattention") %>%
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
  
save_as_docx(ft_tbl_mediation_inat, path = "./tables/Table_S10_mediation_effects_inat.docx")

# Table S11 - hyperactivity symptoms
(ft_tbl_mediation_hyp <- tbl_mediation %>% 
    filter(outcome=="Hyperactivity") %>%
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

save_as_docx(ft_tbl_mediation_hyp, path = "./tables/Table_S11_mediation_effects_hyp.docx")

# Table S12 - conduct symptoms
(ft_tbl_mediation_con <- tbl_mediation %>% 
    filter(outcome=="Conduct") %>%
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

save_as_docx(ft_tbl_mediation_con, path = "./tables/Table_S12_mediation_effects_con.docx")

# Table S13 - oppositional defiant symptoms
(ft_tbl_mediation_odd <- tbl_mediation %>% 
    filter(outcome=="Oppositional") %>%
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

save_as_docx(ft_tbl_mediation_odd, path = "./tables/Table_S13_mediation_effects_odd.docx")

# Table S14 - depressive symptoms
(ft_tbl_mediation_dep <- tbl_mediation %>% 
    filter(outcome=="Depression") %>%
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

save_as_docx(ft_tbl_mediation_dep, path = "./tables/Table_S14_mediation_effects_dep.docx")

# Table S15 - anxiety symptoms
(ft_tbl_mediation_anx <- tbl_mediation %>% 
    filter(outcome=="Anxiety") %>%
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

save_as_docx(ft_tbl_mediation_anx, path = "./tables/Table_S15_mediation_effects_anx.docx")

# make sign positive and sum
sympt_both_sum <- sympt_both_ests %>%
  mutate(bp_est = est^2) %>%
  group_by(effect) %>%
  summarise(bp_est = sum(bp_est),
            .groups = "drop") %>% 
  as.data.frame()

sympt_both_sum <- sympt_both_sum %>% 
  mutate(perc = `bp_est` / sum(`bp_est`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
sympt_both_sum$ymax <- cumsum(sympt_both_sum$perc)

# compute the bottom of each rectangle
sympt_both_sum$ymin <- c(0, head(sympt_both_sum$ymax, n=-1))

# compute label position
sympt_both_sum$labelPosition <- (sympt_both_sum$ymax + sympt_both_sum$ymin) / 2

# create thin donut chart
dp<-ggplot(sympt_both_sum, aes(ymax=ymax, ymin=ymin, xmax=3.9, xmin=3, fill=effect)) +
     geom_rect() +
     geom_text( x=5, aes(y=labelPosition, label=labels, color=effect), size=4,show.legend = F) +
     geom_text( x=-1.1,y=-0.5, aes(label="Symptoms"),color="black", size = 5, show.legend=FALSE) +
     scale_colour_manual(values= c("#0072B2","#D55E00","#99CCCC")) +
     scale_fill_manual(values= c("#0072B2","#D55E00","#99CCCC")) +
     guides(fill=guide_legend()) +
     coord_polar(theta="y",clip = 'off') +
     xlim(c(-1, 4)) +
     theme_void() +
     theme(legend.position = "bottom",
           legend.title = element_blank(),
           legend.text = element_text(size=13.3),
           plot.margin=unit(c(0,0.6,0,0), "cm"))
dp
