#03.4_plot_genetic_correlations.R

## This script plots genetic correlations of the 11 psychiatric and
## neurodevelopmental conditions with differentiation and total problems. 

library(data.table)
library(tidyverse)
library(patchwork)
library(corrplot)

#load LDSC output for 11 psychiatric conditions with intercept rgs + pvals (diff and tot)
ldsc_diff_int <- fread("N:/durable/projects/differentiation_genomics/GWAS/output_genetic_correlations_new/pgc_diff_int_rgs_constrained_new.txt") 
ldsc_tot_int <- fread("N:/durable/projects/differentiation_genomics/GWAS/output_genetic_correlations_new/pgc_tot_int_rgs_constrained_new.txt") 

#process LDSC output and FDR adjust p-values
ldsc_diff_int <- ldsc_diff_int %>%
  select(-c(h2_int_se,gcov_int_se,p1)) %>%
  mutate(rg_uci = rg+1.96*se,
         rg_lci = rg-1.96*se,
         p2 = str_remove(p2,".sumstats.gz"),
         FDR_pval = p.adjust(p, method="fdr"),
         model = "diff")

ldsc_tot_int <- ldsc_tot_int %>%
  select(-c(h2_int_se,gcov_int_se,p1)) %>%
  mutate(rg_uci = rg+1.96*se,
         rg_lci = rg-1.96*se,
         p2 = str_remove(p2,".sumstats.gz"),
         FDR_pval = p.adjust(p, method="fdr"),
         model = "tot")

ldsc_both <- ldsc_diff_int %>%
  full_join(ldsc_tot_int) %>%
  rename(predictor = p2) %>%
  mutate(predictor = as.factor(predictor)) %>%
  mutate(predictor = fct_relevel(predictor,"asd","adhd","scz","bip","mdd","anx",
                                 "alc","ptsd","ocd","an","ts"))

ldsc_both2 <- ldsc_both %>%
  select(predictor, model, rg, rg_lci, rg_uci, FDR_pval) %>%
  mutate(fdr_sig = ifelse(FDR_pval<.05, "FDR p < .05", "FDR p > .05")) %>%
  mutate(fdr_sig = as.factor(fdr_sig)) %>%
  mutate(fdr_sig=fct_relevel(fdr_sig,"FDR p > .05","FDR p < .05"))

# create corrplot ####

# get diff rgs
ldsc_diff <- ldsc_both2 %>%
  filter(model=="diff") %>%
  as.data.frame()

ldsc_diff_rg <- ldsc_diff[2:3]

colnames(ldsc_diff_rg) <- c("model","int_diff")

ldsc_diff_rg <- as.data.frame(ldsc_diff_rg) %>%
  select(int_diff,model)

#get diff FDR p-values
ldsc_diff_p <- ldsc_diff[6] %>%
  as.data.frame()

# get tot rgs
ldsc_tot <- ldsc_both2 %>%
  filter(model=="tot") %>%
  as.data.frame()

ldsc_tot_rg <- ldsc_tot[2:3]

colnames(ldsc_tot_rg) <- c("model","int_tot")

ldsc_tot_rg <- as.data.frame(ldsc_tot_rg) %>%
  select(int_tot,model)

#get tot FDR p-values
ldsc_tot_p <- ldsc_tot[6] %>%
  as.data.frame()

# create df for p-values
names <- c("Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD","Anxiety")

ldsc_diff_p <- ldsc_diff_p %>%
  rename(FDR_pval_diff = FDR_pval) %>%
  cbind(names) %>%
  pivot_wider(values_from = FDR_pval_diff,
              names_from = names) %>%
  as.data.frame()

ldsc_tot_p <- ldsc_tot_p %>%
  rename(FDR_pval_tot = FDR_pval) %>%
  cbind(names) %>%
  pivot_wider(values_from = FDR_pval_tot,
              names_from = names) %>%
  as.data.frame()

ldsc_both_p <- ldsc_diff_p %>%
  rbind(ldsc_tot_p)

rownames(ldsc_both_p) <- c("Differentiation","Total problems")

# create rg df for plotting
ldsc_plot_diff <- ldsc_diff_rg %>%
  select(-model) %>%
  cbind(names) %>%
  pivot_wider(values_from = int_diff,
              names_from = names) %>%
  as.data.frame()

ldsc_plot_tot <- ldsc_tot_rg %>%
  select(-model) %>%
  cbind(names) %>%
  pivot_wider(values_from = int_tot,
              names_from = names) %>%
  as.data.frame()

ldsc_plot_both <- ldsc_plot_diff %>%
  rbind(ldsc_plot_tot)

rownames(ldsc_plot_both) <- c("Differentiation","Total problems")

ldsc_plot_both[ldsc_plot_both > 1] <- 1

par(xpd=TRUE)

col1 = colorRampPalette(
  c('#800000','#FFFFFF', '#028A0F'))

ldsc_both = wrap_elements(~corrplot(as.matrix(ldsc_plot_both), p.mat = as.matrix(ldsc_both_p),            #data to use
                                    sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',                #FDR p-values
                                    tl.cex = 1.7, tl.offset = 1, tl.srt = 45, tl.col = "black",           #text settings
                                    col.lim = c(-1,1), cl.length = 11, cl.cex = 1.5, col = col1(          #colour legend
                                    200), cl.pos = 'b', cl.align.text = 'l', cl.ratio = 0.5,              #colour legend
                                    pch.cex = 1.5, is.corr = FALSE, outline = TRUE, bg = 'white',         #other settings
                                    mar = c(1,1,1,1)))                                                    #other settings
ldsc_both

#load LDSC output for 11 psychiatric conditions with slope rgs + pvals (diff and tot)
ldsc_diff_slope <- fread("N:/durable/projects/differentiation_genomics/scripts/GWAS/genetic_correlations/pgc_diff_slope_rgs_constrained.txt") 
ldsc_tot_slope <- fread("N:/durable/projects/differentiation_genomics/scripts/GWAS/genetic_correlations/pgc_tot_slope_rgs_constrained.txt") 

#process LDSC output and FDR adjust p-values
ldsc_diff_slope <- ldsc_diff_slope %>%
  select(-c(h2_int_se,gcov_int_se,p1)) %>%
  mutate(rg_uci = rg+1.96*se,
         rg_lci = rg-1.96*se,
         p2 = str_remove(p2,".sumstats.gz"),
         FDR_pval = p.adjust(p, method="fdr"),
         model = "diff") %>%
  filter(p2 != "anx")

ldsc_tot_slope <- ldsc_tot_slope %>%
  select(-c(h2_int_se,gcov_int_se,p1)) %>%
  mutate(rg_uci = rg+1.96*se,
         rg_lci = rg-1.96*se,
         p2 = str_remove(p2,".sumstats.gz"),
         FDR_pval = p.adjust(p, method="fdr"),
         model = "tot")

ldsc_slope <- ldsc_diff_slope %>%
  full_join(ldsc_tot_slope) %>%
  rename(predictor = p2) %>%
  mutate(predictor = as.factor(predictor)) %>%
  mutate(predictor = fct_relevel(predictor,"asd","adhd","scz","bip","mdd",
                                 "alc","ptsd","ocd","an","ts"))

ldsc_slope2 <- ldsc_slope %>%
  select(predictor, model, rg, rg_lci, rg_uci, FDR_pval) %>%
  mutate(fdr_sig = ifelse(FDR_pval<.05, "FDR p < .05", "FDR p > .05")) %>%
  mutate(fdr_sig = as.factor(fdr_sig)) %>%
  mutate(fdr_sig=fct_relevel(fdr_sig,"FDR p > .05","FDR p < .05"))

# create corrplot ####

# get diff rgs
ldsc_slope_diff <- ldsc_slope2 %>%
  filter(model=="diff") %>%
  as.data.frame()

ldsc_diff_slope_rg <- ldsc_slope_diff[2:3]

colnames(ldsc_diff_slope_rg) <- c("model","slope_diff")

ldsc_diff_slope_rg <- as.data.frame(ldsc_diff_slope_rg) %>%
  select(slope_diff,model)

#get diff FDR p-values
ldsc_diff_slope_p <- ldsc_slope_diff[6] %>%
  as.data.frame()

# get tot rgs
ldsc_slope_tot <- ldsc_slope2 %>%
  filter(model=="tot") %>%
  as.data.frame()

ldsc_tot_slope_rg <- ldsc_slope_tot[2:3]

colnames(ldsc_tot_slope_rg) <- c("model","slope_tot")

ldsc_tot_slope_rg <- as.data.frame(ldsc_tot_slope_rg) %>%
  select(slope_tot,model)

#get tot FDR p-values
ldsc_tot_slope_p <- ldsc_slope_tot[6] %>%
  as.data.frame()

# create df for p-values
names <- c("Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD")

ldsc_diff_slope_p <- ldsc_diff_slope_p %>%
  rename(FDR_pval_diff = FDR_pval) %>%
  cbind(names) %>%
  pivot_wider(values_from = FDR_pval_diff,
              names_from = names) %>%
  as.data.frame()

ldsc_tot_slope_p <- ldsc_tot_slope_p %>%
  rename(FDR_pval_tot = FDR_pval) %>%
  cbind(names) %>%
  pivot_wider(values_from = FDR_pval_tot,
              names_from = names) %>%
  as.data.frame()

ldsc_slope_p <- ldsc_diff_slope_p %>%
  rbind(ldsc_tot_slope_p)

rownames(ldsc_slope_p) <- c("Differentiation","Total problems")

# create rg df for plotting
ldsc_plot_diff_slope <- ldsc_diff_slope_rg %>%
  select(-model) %>%
  cbind(names) %>%
  pivot_wider(values_from = slope_diff,
              names_from = names) %>%
  as.data.frame()

ldsc_plot_tot_slope <- ldsc_tot_slope_rg %>%
  select(-model) %>%
  cbind(names) %>%
  pivot_wider(values_from = slope_tot,
              names_from = names) %>%
  as.data.frame()

ldsc_plot_both_slope <- ldsc_plot_diff_slope %>%
  rbind(ldsc_plot_tot_slope)

rownames(ldsc_plot_both_slope) <- c("Differentiation","Total problems")

ldsc_plot_both_slope[ldsc_plot_both_slope > 1] <- 1

par(xpd=TRUE)

col1 = colorRampPalette(
  c('#800000','#FFFFFF', '#028A0F'))

ldsc_slope = wrap_elements(~corrplot(as.matrix(ldsc_plot_both_slope), p.mat = as.matrix(ldsc_slope_p),    #data to use
                                    sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',                #FDR p-values
                                    tl.cex = 1.7, tl.offset = 1, tl.srt = 45, tl.col = "black",           #text settings
                                    col.lim = c(-1,1), cl.length = 11, cl.cex = 1.5, col = col1(          #colour legend
                                      200), cl.pos = 'b', cl.align.text = 'l', cl.ratio = 0.5,            #colour legend
                                    pch.cex = 1.5, is.corr = FALSE, outline = TRUE, bg = 'white',         #other settings
                                    mar = c(1,1,1,1)))                                                    #other settings
ldsc_slope

# create corrplot 11 psychiatric conditions
load(file="./data/cov_wpsych.RData")

cov_plot <- cov_wpsych$S_Stand %>%
  as.data.frame()

cov_plot <- cov_plot[1:11, 1:11]

rownames(cov_plot) <- c("Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD","Anxiety")
colnames(cov_plot) <- c("Anorexia","OCD","Tourette's","SCZ","Bipolar","Alcohol","ADHD","Autism","PTSD","MDD","Anxiety")

cov_plot[cov_plot > 1] <- 1

col1 = colorRampPalette(
  c('#800000','#FFFFFF', '#028A0F'))

# wrap so the plot can be included in a patchwork
cov = wrap_elements(~corrplot(as.matrix(cov_plot), method = 'color', 
                              tl.cex = 1.3, tl.offset = 0.5, tl.srt = 45, tl.col = "black",          #text settings
                              col.lim = c(-1,1), cl.length = 11, cl.cex = 1.1, col=col1(200),        #colour legend 
                              cl.pos = 'r', cl.align.text = 'l', cl.ratio = 0.15,                    #colour legend
                              is.corr = FALSE, outline = TRUE, bg = 'white'))
cov
