#07_run_equivalence_tests.R

#This script runs equivalence tests to determine, in conjunction with
#null hypothesis significance tests (NHST), whether PGS effects are:

## a) null (within the region of practical equivalence to zero); 
## b) non-null (statistically significant and not entirely within 
##    the region of practical equivalence to zero); or
## c) undecided (not statistically significant and not entirely within 
##    the region of practical equivalence to zero). 

library(tidyverse)
library(effectsize)
library(ggrepel)

# load data ----

# unadjusted PGS results
load(file="./output/fit_model1_both_ld.RData")
load(file="./output/unadjusted_PGS_ests.RData")

#trio PGS results
load("./output/fit_triomodel_full_ld.RData")
load("./output/trio_PGS_effects.RData")

# get 90% CIs and df from unadjusted model
tmp <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.9) %>%
  filter(lhs=="i1"|lhs=="i2",
         op == "~") %>%
  mutate(df=fitMeasures(fit_model1_both, "df"))

# remove results for covariates
tmp2 <- tmp[-c(1,2), ]

# add 90% CIs and df to the processed results
equiv_ests <- ests %>%
  rename(LCI_95=ci.lower,UCI_95=ci.upper) %>%
  cbind(tmp2$df) %>%
  cbind(tmp2$ci.lower) %>%
  cbind(tmp2$ci.upper) %>%
  rename(est=est.std,df=`tmp2$df`,LCI_90=`tmp2$ci.lower`,UCI_90=`tmp2$ci.upper`) %>%
  mutate(nhst_sig = ifelse(FDR_pval<.05, 1, 0))

# get 90% CIs and df from trio model
tmp3 <- standardizedSolution(fit_triomodel_full, ci = TRUE, level = 0.9) %>%
  filter(lhs=="i1"|lhs=="i2",
         op == "~") %>%
  mutate(df=fitMeasures(fit_triomodel_full, "df"))

# remove results for covariates
tmp4 <- tmp3[-c(1,2), ]

# add 90% CIs and df to the processed trio results
equiv_ests_trio <- trio_both %>%
  rename(LCI_95=ci.lower,UCI_95=ci.upper) %>%
  cbind(tmp4$df) %>%
  cbind(tmp4$ci.lower) %>%
  cbind(tmp4$ci.upper) %>%
  rename(est=est.std,df=`tmp4$df`,LCI_90=`tmp4$ci.lower`,UCI_90=`tmp4$ci.upper`) %>%
  mutate(nhst_sig = ifelse(fdr_pval<.05, 1, 0))

# make helper functions (based on equivalence_test.lm in parameters package) 

.rope_coverage <- function(rope, ci) {
  diff_rope <- abs(diff(rope))
  diff_ci <- abs(diff(ci))
  
  # inside?
  if (min(ci) >= min(rope) && max(ci) <= max(rope)) {
    coverage <- 1
    
    # outside?
  } else if (max(ci) < min(rope) || min(ci) > max(rope)) {
    coverage <- 0
    
    # CI covers completely rope?
  } else if (max(ci) > max(rope) && min(ci) < min(rope)) {
    coverage <- diff_rope / diff_ci
    
    # CI inside rope and outside max rope?
  } else if (min(ci) >= min(rope) && max(ci) > max(rope)) {
    diff_in_rope <- max(rope) - min(ci)
    coverage <- diff_in_rope / diff_ci
    
    # CI inside rope and outside min rope?
  } else if (max(ci) <= max(rope) && min(ci) < min(rope)) {
    diff_in_rope <- max(ci) - min(rope)
    coverage <- diff_in_rope / diff_ci
  }
  
  coverage
}


# make equiv_test function -----

equiv_test<-function(est,se,df,sesoi,ci,nhst_sig){
  
  rope <- c(-sesoi,sesoi)
  
  coverage <- .rope_coverage(rope,ci)
  
  # ==== Lakens' rule for decision ====
  
     range_rope <- rope
    # significant result?
    if (nhst_sig==1) {
      # check if CI are entirely inside ROPE. If CI crosses ROPE, reject H0, else accept
      if (min(abs(ci)) < max(abs(rope)) && max(abs(ci)) < max(abs(rope))) {
        decision <- "Null"
      } else {
        decision <- "Non-null"
      }
      # non-significant results
    } else {
      # check if CI are entirely inside ROPE. If CI crosses ROPE, reject H0, else accept
      if (min(abs(ci)) < max(abs(rope)) && max(abs(ci)) < max(abs(rope))) {
        decision <- "Null"
      } else {
        decision <- "Undecided"
      }
    }
  
  # perform the test
  ttest <- function(est,se,df){
    if(est < rope[[1]] | est > rope[[2]]){
      1 
    } else {
      tstat1 <- (est-rope[[1]])/se
      tstat2 <- (est-rope[[2]])/se
      max(pt(abs(tstat1), df, lower.tail = FALSE), pt(abs(tstat2), df, lower.tail = FALSE) )
    }
    
  }
  
  equiv_p <- ttest(est=est,se=se,df=df)
  
  tibble::tibble("Estimate" = est,
                 "LCI_90" = ci[[1]],
                 "UCI_90" = ci[[2]],
                 "ROPE_range" = paste0(round(rope,3),collapse=":"),
                 "SGPV" = coverage,
                 "TOST_p" = equiv_p,
                 "NHST+Equiv" = decision)
  
}

# convert SESOI ----

sesoi= d_to_r(0.1)

# run equivalence test unadjusted PGS results -----

dat <- equiv_ests

res=data.frame()
for(i in 1:nrow(dat)){
  
  tmpdat <- dat[i,]
  tmpres<- equiv_test(est=tmpdat$est,
                      se=tmpdat$se,
                      sesoi=sesoi,
                      df=tmpdat$df,
                      ci=c(tmpdat$LCI_90,tmpdat$UCI_90), 
                      nhst_sig=tmpdat$nhst_sig)
  
  res <- rbind(res,tmpres)
}

view(res)

# run equivalence test trio-adjusted direct effect results -----

dat2 <- equiv_ests_trio %>%
  filter(Effect=="Direct\n (cPGS)")

trio_res=data.frame()
for(i in 1:nrow(dat2)){
  
  tmpdat2 <- dat2[i,]
  tmpres2<- equiv_test(est=tmpdat2$est,
                       se=tmpdat2$se,
                       sesoi=sesoi,
                       df=tmpdat2$df,
                       ci=c(tmpdat2$LCI_90,tmpdat2$UCI_90), 
                       nhst_sig=tmpdat2$nhst_sig)
  
  trio_res <- rbind(trio_res,tmpres2)
}

view(trio_res)

# run equivalence test indirect genetic effect results -----

dat3 <- equiv_ests_trio %>%
  filter(Effect=="Indirect\n (mPGS + fPGS)")

trio_ind_res=data.frame()
for(i in 1:nrow(dat3)){
  
  tmpdat3 <- dat3[i,]
  tmpres3<- equiv_test(est=tmpdat3$est,
                       se=tmpdat3$se,
                       sesoi=sesoi,
                       df=tmpdat3$df,
                       ci=c(tmpdat3$LCI_90,tmpdat3$UCI_90), 
                       nhst_sig=tmpdat3$nhst_sig)
  
  trio_ind_res <- rbind(trio_ind_res,tmpres3)
}

view(trio_ind_res)

# merge unadjusted equivalence test output with
# original unadjusted results, and FDR adjust
final_res <- dat %>% 
  select(-c(LCI_90,UCI_90)) %>%
  bind_cols(res) %>%
  mutate(FDR_TOST_p=p.adjust(TOST_p,method="fdr"))

# update NHST+Equiv decision based on FDR-adjusted p values for 
# NHST+Equiv, since the 90% confidence intervals are not FDR adjusted
final_res <- final_res %>%
  mutate(`NHST+Equiv`= case_when(nhst_sig==0 & FDR_TOST_p<.05 ~ "Null",
                                 nhst_sig==1 & FDR_TOST_p<.05 ~ "Null",
                                 nhst_sig==1 & FDR_TOST_p>.05 ~ "Non-null",
                                 nhst_sig==0 & FDR_TOST_p>.05 ~ "Undecided"))

# merge trio model equivalence test output with
# original trio-adjusted results, and FDR adjust
final_trio_res <- dat2 %>% 
  select(-c(LCI_90,UCI_90)) %>%
  bind_cols(trio_res) %>%
  mutate(FDR_TOST_p=p.adjust(TOST_p,method="fdr"))

# update NHST+Equiv decision based on FDR-adjusted p values,
# since the 90% confidence intervals are cannot be FDR adjusted
final_trio_res <- final_trio_res %>%
  mutate(`NHST+Equiv`= case_when(nhst_sig==0 & FDR_TOST_p<.05 ~ "Null",
                                 nhst_sig==1 & FDR_TOST_p<.05 ~ "Null",
                                 nhst_sig==1 & FDR_TOST_p>.05 ~ "Non-null",
                                 nhst_sig==0 & FDR_TOST_p>.05 ~ "Undecided"))

# merge trio model indirect effects equivalence test output 
# with original trio model results, and FDR adjust
final_trio_ind_res <- dat3 %>% 
  select(-c(LCI_90,UCI_90)) %>%
  bind_cols(trio_ind_res) %>%
  mutate(FDR_TOST_p=p.adjust(TOST_p,method="fdr"))

# update NHST+Equiv decision based on FDR-adjusted p values,
# since the 90% confidence intervals are cannot be FDR adjusted
final_trio_ind_res <- final_trio_ind_res %>%
  mutate(`NHST+Equiv`= case_when(nhst_sig==0 & FDR_TOST_p<.05 ~ "Null",
                                 nhst_sig==1 & FDR_TOST_p<.05 ~ "Null",
                                 nhst_sig==1 & FDR_TOST_p>.05 ~ "Non-null",
                                 nhst_sig==0 & FDR_TOST_p>.05 ~ "Undecided"))

# tabulate ----

knitr::kable(final_res %>%
               select("PGS"=pgs,"Outcome"=outcome,Estimate,"90% LCI"=LCI_90,
                      "90% UCI"=UCI_90,"Equiv. p value"=FDR_TOST_p,`NHST+Equiv`),
               digits=3)

knitr::kable(final_trio_res %>%
               select("PGS"=pgs,"Outcome"=model,Estimate,"90% LCI"=LCI_90,
                      "90% UCI"=UCI_90,"Equiv. p value"=FDR_TOST_p,`NHST+Equiv`),
             digits=3)

knitr::kable(final_trio_ind_res %>%
               select("PGS"=pgs,"Outcome"=model,Estimate,"90% LCI"=LCI_90,
                      "90% UCI"=UCI_90,"Equiv. p value"=FDR_TOST_p,`NHST+Equiv`),
             digits=3)

# plot -----

ROPE_high= effectsize::d_to_r(0.1)
ROPE_low=-ROPE_high

# prepare unadjusted PGS output for plotting
plot_res <- final_res %>%
  select(pgs,outcome,`NHST+Equiv`,FDR_TOST_p, LCI_90,UCI_90) %>%
  gather(ci,value,-pgs:-FDR_TOST_p) %>% 
  mutate(value=abs(value)) %>% 
  group_by(pgs,outcome,`NHST+Equiv`,FDR_TOST_p) %>% 
  summarise(max = max(value)) %>%
  mutate(pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                         "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

## plot equiv test unadjusted PGS results
px<-ggplot(plot_res,aes(x=max, y=pgs)) +
  geom_vline(aes(xintercept=ROPE_high), color = "grey60", linetype = 2, size=0.7) + 
  geom_point(aes(shape= `NHST+Equiv`, fill=log10(FDR_TOST_p)),size=4,stroke=0.8) +
  scale_shape_manual(values=c(24,21,22), name = "NHST+Equiv.\nconclusion" )+
  facet_grid(cols=vars(outcome)) + 
  theme_classic(base_size = 14) +
  labs(shape="Best-fitting\nmodel")+ 
  scale_fill_viridis_c("Equiv. test\np value",breaks=c(log10(0.000005), log10(0.05)),
                       labels=c("5x10-6","0.05") ,option="magma", begin=0.45, end=1, direction=-1)+
  scale_x_continuous("Most extreme absolute value\nwithin 90% CI range",  ) +
  scale_y_discrete("Genetic liability to psychiatric conditions",limits=rev) +
  coord_cartesian(xlim=c(0,0.127))+
  theme(strip.text.y = element_text(angle=0),
        strip.text = element_text(size=15, colour = 'black'),
        axis.text.x = element_text(angle=90,size=12,colour="black"),
        axis.text.y = element_text(size=13,colour="black"),
        axis.title = element_text(size=13.5,colour="black"),
        axis.line = element_line(colour="grey50",size=0),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        legend.position= "right",
        legend.direction = "vertical")

print(px)

# prepare trio output for plotting
plot_trio_res <- final_trio_res %>%
  select(pgs,model,`NHST+Equiv`,FDR_TOST_p, LCI_90,UCI_90) %>%
  gather(ci,value,-pgs:-FDR_TOST_p) %>% 
  mutate(value=abs(value)) %>% 
  group_by(pgs,model,`NHST+Equiv`,FDR_TOST_p) %>% 
  summarise(max = max(value)) %>%
  mutate(pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                         "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

## plot equiv test trio-adjusted PGS results
trio<-ggplot(plot_trio_res,aes(x=max, y=pgs)) +
  geom_vline(aes(xintercept=ROPE_high), color = "grey60", linetype = 2, size=0.7) + 
  geom_point(aes(shape= `NHST+Equiv`, fill=log10(FDR_TOST_p)),size=4,stroke=0.8) +
  scale_shape_manual(values=c(24,21,22), name = "NHST+Equiv.\nconclusion" )+
  facet_grid(cols=vars(model)) + 
  theme_classic(base_size = 14) +
  labs(shape="Best-fitting\nmodel")+ 
  scale_fill_viridis_c("Equiv. test\np value",breaks=c(log10(0.005), log10(0.05)),
                       labels=c("0.005","0.05") ,option="magma", begin=0.45, end=1, direction=-1)+
  scale_x_continuous("Most extreme absolute value\nwithin 90% CI range",  ) +
  scale_y_discrete("Genetic liability to psychiatric conditions",limits=rev) +
  coord_cartesian(xlim=c(0,0.137))+
  theme(strip.text.y = element_text(angle=0),
        strip.text = element_text(size=15, colour = 'black'),
        axis.text.x = element_text(angle=90,size=12,colour="black"),
        axis.text.y = element_text(size=13,colour="black"),
        axis.title = element_text(size=13.5,colour="black"),
        axis.line = element_line(colour="grey50",size=0),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        legend.position= "right",
        legend.direction = "vertical")

print(trio)

# prepare maternal indirect effect output for plotting
final_trio_ind_res_m <- final_trio_ind_res[grepl("_mother", final_trio_ind_res$rhs),]

plot_trio_ind_res_m <- final_trio_ind_res_m %>%
  select(pgs,model,`NHST+Equiv`,FDR_TOST_p, LCI_90,UCI_90) %>%
  gather(ci,value,-pgs:-FDR_TOST_p) %>% 
  mutate(value=abs(value)) %>% 
  group_by(pgs,model,`NHST+Equiv`,FDR_TOST_p) %>% 
  summarise(max = max(value)) %>%
  mutate(pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                         "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

## plot equiv test maternal indirect genetic effects from trio model
ind_m<-ggplot(plot_trio_ind_res_m,aes(x=max, y=pgs)) +
  geom_vline(aes(xintercept=ROPE_high), color = "grey60", linetype = 2, size=0.7) + 
  geom_point(aes(shape= `NHST+Equiv`, fill=log10(FDR_TOST_p)),size=4,stroke=0.8) +
  scale_shape_manual(values=c(24,21), name = "NHST+Equiv.\nconclusion" )+
  facet_grid(cols=vars(model)) + 
  theme_classic(base_size = 14) +
  labs(shape="Best-fitting\nmodel")+ 
  scale_fill_viridis_c("Equiv. test\np value",breaks=c(log10(0.00005), log10(0.05)),
                       labels=c("10x5-5","0.05") ,option="magma", begin=0.45, end=1, direction=-1)+
  scale_x_continuous("Most extreme absolute value\nwithin 90% CI range",  ) +
  scale_y_discrete("Genetic liability to psychiatric conditions",limits=rev) +
  coord_cartesian(xlim=c(0,0.1))+
  theme(strip.text.y = element_text(angle=0),
        strip.text = element_text(size=15, colour = 'black'),
        axis.text.x = element_text(angle=90,size=12,colour="black"),
        axis.text.y = element_text(size=13,colour="black"),
        axis.title = element_text(size=13.5,colour="black"),
        axis.line = element_line(colour="grey50",size=0),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        legend.position= "right",
        legend.direction = "vertical")

print(ind_m)

# prepare paternal indirect effect output for plotting
final_trio_ind_res_f <- final_trio_ind_res[grepl("_father", final_trio_ind_res$rhs),]

plot_trio_ind_res_f <- final_trio_ind_res_f %>%
  select(pgs,model,`NHST+Equiv`,FDR_TOST_p, LCI_90,UCI_90) %>%
  gather(ci,value,-pgs:-FDR_TOST_p) %>% 
  mutate(value=abs(value)) %>% 
  group_by(pgs,model,`NHST+Equiv`,FDR_TOST_p) %>% 
  summarise(max = max(value)) %>%
  mutate(pgs=fct_relevel(pgs,"Anorexia","OCD","Tourette's","SCZ","Bipolar",
                         "Alcohol","ADHD","Autism","PTSD","MDD","Anxiety"))

## plot equiv test maternal indirect genetic effects from trio model
ind_f<-ggplot(plot_trio_ind_res_f,aes(x=max, y=pgs)) +
  geom_vline(aes(xintercept=ROPE_high), color = "grey60", linetype = 2, size=0.7) + 
  geom_point(aes(shape= `NHST+Equiv`, fill=log10(FDR_TOST_p)),size=4,stroke=0.8) +
  scale_shape_manual(values=c(21), name = "NHST+Equiv.\nconclusion" )+
  facet_grid(cols=vars(model)) + 
  theme_classic(base_size = 14) +
  labs(shape="Best-fitting\nmodel")+ 
  scale_fill_viridis_c("Equiv. test\np value",breaks=c(log10(0.00005), log10(0.005), log10(0.05)),
                       labels=c("10x5-5","0.005","0.05") ,option="magma", begin=0.45, end=1, direction=-1)+
  scale_x_continuous("Most extreme absolute value\nwithin 90% CI range",  ) +
  scale_y_discrete("Genetic liability to psychiatric conditions",limits=rev) +
  coord_cartesian(xlim=c(0,0.1))+
  theme(strip.text.y = element_text(angle=0),
        strip.text = element_text(size=15, colour = 'black'),
        axis.text.x = element_text(angle=90,size=12,colour="black"),
        axis.text.y = element_text(size=13,colour="black"),
        axis.title = element_text(size=13.5,colour="black"),
        axis.line = element_line(colour="grey50",size=0),
        strip.background = element_rect(colour="black",fill="white"),
        panel.border = element_rect(colour="grey50",fill=NA),
        legend.position= "right",
        legend.direction = "vertical")

print(ind_f)
