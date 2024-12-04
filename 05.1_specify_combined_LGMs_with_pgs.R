#05.1_specify_combined_LGMs_with_pgs.R

#This script specifies a basic LGM and three versions of the LGM including child 
#PGS as predictors and differentiation and total problems as outcomes (combined).
#The models are run by the script: '05_run_combined_LGMs_with_pgs.R'.

## 1. basic latent growth model (LGM)
## 2. LGM with PGS as predictors (intercept and slope)
## 3. effects on intercepts only
## 4. effects on slopes only

#basic LGM

model_basic <-  
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
    
           i1 ~~ i1
           s1 ~~ s1
           i1 ~~ s1
           i2 ~~ i2
           s2 ~~ s2
           i2 ~~ s2
           i1 ~~ i2
           i1 ~~ s2
           s1 ~~ i2
           s1 ~~ s2
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
           
# growth parameter intercepts (freely estimated)
  
           i1 ~ 1 
           s1 ~ 1
           i2 ~ 1
           s2 ~ 1

# time-invariant covariate (effects)

           i1 + s1 ~ sex
           i2 + s2 ~ sex

# time-invariant covariate (intercept & variance)

           sex ~~ sex
           sex ~ 1
           '


# both: LGM with covariates and predictors

  model1 <-  
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
    
           i1 ~~ i1
           s1 ~~ s1
           i1 ~~ s1
           i2 ~~ i2
           s2 ~~ s2
           i2 ~~ s2
           i1 ~~ i2
           i1 ~~ s2
           s1 ~~ i2
           s1 ~~ s2
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
           
# growth parameter intercepts (freely estimated)
  
           i1 ~ 1 
           s1 ~ 1
           i2 ~ 1
           s2 ~ 1

# time-invariant covariate (effects)

           i1 + s1 ~ sex
           i2 + s2 ~ sex

# time-invariant covariate (intercept & variance)

           sex ~~ sex
           sex ~ 1

# predictors (effects)

           i1 + s1 ~ adhd_child + asd_child + scz_child + bip_child + mdd_child + anx_child + alc_child + ptsd_child + ocd_child + an_child + ts_child
           i2 + s2 ~ adhd_child + asd_child + scz_child + bip_child + mdd_child + anx_child + alc_child + ptsd_child + ocd_child + an_child + ts_child
                   
# predictors (intercept & variance)

           adhd_child ~~ adhd_child
           asd_child ~~ asd_child
           scz_child ~~ scz_child
           bip_child ~~ bip_child
           mdd_child ~~ mdd_child
           anx_child ~~ anx_child
           alc_child ~~ alc_child
           ptsd_child ~~ ptsd_child
           ocd_child ~~ ocd_child
           an_child ~~ an_child
           ts_child ~~ ts_child
           adhd_child ~ 1
           asd_child ~ 1
           scz_child ~ 1
           bip_child ~ 1
           mdd_child ~ 1
           anx_child ~ 1
           alc_child ~ 1
           ptsd_child ~ 1
           ocd_child ~ 1
           an_child ~ 1
           ts_child ~ 1

# covariances

           sex ~~ anx_child
           sex ~~ alc_child
           sex ~~ ptsd_child
           sex ~~ ocd_child
           sex ~~ an_child
           sex ~~ adhd_child
           sex ~~ asd_child
           sex ~~ scz_child
           sex ~~ bip_child
           sex ~~ mdd_child
           sex ~~ ts_child
           adhd_child ~~ asd_child
           adhd_child ~~ scz_child
           adhd_child ~~ bip_child
           adhd_child ~~ mdd_child
           adhd_child ~~ anx_child
           adhd_child ~~ alc_child
           adhd_child ~~ ptsd_child
           adhd_child ~~ ocd_child
           adhd_child ~~ an_child
           adhd_child ~~ ts_child
           asd_child ~~ scz_child
           asd_child ~~ bip_child
           asd_child ~~ mdd_child
           asd_child ~~ anx_child
           asd_child ~~ alc_child
           asd_child ~~ ptsd_child
           asd_child ~~ ocd_child
           asd_child ~~ an_child
           asd_child ~~ ts_child
           scz_child ~~ bip_child
           scz_child ~~ mdd_child
           scz_child ~~ anx_child
           scz_child ~~ alc_child
           scz_child ~~ ptsd_child
           scz_child ~~ ocd_child
           scz_child ~~ an_child
           scz_child ~~ ts_child
           bip_child ~~ mdd_child
           bip_child ~~ anx_child
           bip_child ~~ alc_child
           bip_child ~~ ptsd_child
           bip_child ~~ ocd_child
           bip_child ~~ an_child
           bip_child ~~ ts_child
           mdd_child ~~ anx_child
           mdd_child ~~ alc_child
           mdd_child ~~ ptsd_child
           mdd_child ~~ ocd_child
           mdd_child ~~ an_child
           mdd_child ~~ ts_child
           anx_child ~~ alc_child
           anx_child ~~ ptsd_child
           anx_child ~~ ocd_child
           anx_child ~~ an_child
           anx_child ~~ ts_child
           alc_child ~~ ptsd_child
           alc_child ~~ ocd_child
           alc_child ~~ an_child
           alc_child ~~ ts_child
           ptsd_child ~~ ocd_child
           ptsd_child ~~ an_child
           ptsd_child ~~ ts_child
           ocd_child ~~ an_child
           ocd_child ~~ ts_child
           an_child ~~ ts_child
           '
  
  
  
# both: LGM with covariates and predictors, intercept effects only
  
  model2 <-  
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
    
           i1 ~~ i1
           s1 ~~ s1
           i1 ~~ s1
           i2 ~~ i2
           s2 ~~ s2
           i2 ~~ s2
           i1 ~~ i2
           i1 ~~ s2
           s1 ~~ i2
           s1 ~~ s2
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
           
# growth parameter intercepts (freely estimated)
  
           i1 ~ 1 
           s1 ~ 1
           i2 ~ 1
           s2 ~ 1

# time-invariant covariate (effects)

           i1 ~ sex
           i2 ~ sex

# time-invariant covariate (intercept & variance)

           sex ~~ sex
           sex ~ 1

# predictors (effects)

           i1 ~ adhd_child + asd_child + scz_child + bip_child + mdd_child + anx_child + alc_child + ptsd_child + ocd_child + an_child + ts_child
           i2 ~ adhd_child + asd_child + scz_child + bip_child + mdd_child + anx_child + alc_child + ptsd_child + ocd_child + an_child + ts_child
                   
# predictors (intercept & variance)

           adhd_child ~~ adhd_child
           asd_child ~~ asd_child
           scz_child ~~ scz_child
           bip_child ~~ bip_child
           mdd_child ~~ mdd_child
           anx_child ~~ anx_child
           alc_child ~~ alc_child
           ptsd_child ~~ ptsd_child
           ocd_child ~~ ocd_child
           an_child ~~ an_child
           ts_child ~~ ts_child
           adhd_child ~ 1
           asd_child ~ 1
           scz_child ~ 1
           bip_child ~ 1
           mdd_child ~ 1
           anx_child ~ 1
           alc_child ~ 1
           ptsd_child ~ 1
           ocd_child ~ 1
           an_child ~ 1
           ts_child ~ 1

# covariances

           sex ~~ anx_child
           sex ~~ alc_child
           sex ~~ ptsd_child
           sex ~~ ocd_child
           sex ~~ an_child
           sex ~~ adhd_child
           sex ~~ asd_child
           sex ~~ scz_child
           sex ~~ bip_child
           sex ~~ mdd_child
           sex ~~ ts_child
           adhd_child ~~ asd_child
           adhd_child ~~ scz_child
           adhd_child ~~ bip_child
           adhd_child ~~ mdd_child
           adhd_child ~~ anx_child
           adhd_child ~~ alc_child
           adhd_child ~~ ptsd_child
           adhd_child ~~ ocd_child
           adhd_child ~~ an_child
           adhd_child ~~ ts_child
           asd_child ~~ scz_child
           asd_child ~~ bip_child
           asd_child ~~ mdd_child
           asd_child ~~ anx_child
           asd_child ~~ alc_child
           asd_child ~~ ptsd_child
           asd_child ~~ ocd_child
           asd_child ~~ an_child
           asd_child ~~ ts_child
           scz_child ~~ bip_child
           scz_child ~~ mdd_child
           scz_child ~~ anx_child
           scz_child ~~ alc_child
           scz_child ~~ ptsd_child
           scz_child ~~ ocd_child
           scz_child ~~ an_child
           scz_child ~~ ts_child
           bip_child ~~ mdd_child
           bip_child ~~ anx_child
           bip_child ~~ alc_child
           bip_child ~~ ptsd_child
           bip_child ~~ ocd_child
           bip_child ~~ an_child
           bip_child ~~ ts_child
           mdd_child ~~ anx_child
           mdd_child ~~ alc_child
           mdd_child ~~ ptsd_child
           mdd_child ~~ ocd_child
           mdd_child ~~ an_child
           mdd_child ~~ ts_child
           anx_child ~~ alc_child
           anx_child ~~ ptsd_child
           anx_child ~~ ocd_child
           anx_child ~~ an_child
           anx_child ~~ ts_child
           alc_child ~~ ptsd_child
           alc_child ~~ ocd_child
           alc_child ~~ an_child
           alc_child ~~ ts_child
           ptsd_child ~~ ocd_child
           ptsd_child ~~ an_child
           ptsd_child ~~ ts_child
           ocd_child ~~ an_child
           ocd_child ~~ ts_child
           an_child ~~ ts_child
           '
  
  
  
# both: LGM with covariates and predictors, slope effects only
  
  model3 <-  
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
    
           i1 ~~ i1
           s1 ~~ s1
           i1 ~~ s1
           i2 ~~ i2
           s2 ~~ s2
           i2 ~~ s2
           i1 ~~ i2
           i1 ~~ s2
           s1 ~~ i2
           s1 ~~ s2
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
           
# growth parameter intercepts (freely estimated)
  
           i1 ~ 1 
           s1 ~ 1
           i2 ~ 1
           s2 ~ 1

# time-invariant covariate (effects)

           s1 ~ sex
           s2 ~ sex

# time-invariant covariate (intercept & variance)

           sex ~~ sex
           sex ~ 1

# predictors (effects)

           s1 ~ adhd_child + asd_child + scz_child + bip_child + mdd_child + anx_child + alc_child + ptsd_child + ocd_child + an_child + ts_child
           s2 ~ adhd_child + asd_child + scz_child + bip_child + mdd_child + anx_child + alc_child + ptsd_child + ocd_child + an_child + ts_child
                   
# predictors (intercept & variance)

           adhd_child ~~ adhd_child
           asd_child ~~ asd_child
           scz_child ~~ scz_child
           bip_child ~~ bip_child
           mdd_child ~~ mdd_child
           anx_child ~~ anx_child
           alc_child ~~ alc_child
           ptsd_child ~~ ptsd_child
           ocd_child ~~ ocd_child
           an_child ~~ an_child
           ts_child ~~ ts_child
           adhd_child ~ 1
           asd_child ~ 1
           scz_child ~ 1
           bip_child ~ 1
           mdd_child ~ 1
           anx_child ~ 1
           alc_child ~ 1
           ptsd_child ~ 1
           ocd_child ~ 1
           an_child ~ 1
           ts_child ~ 1

# covariances

           sex ~~ anx_child
           sex ~~ alc_child
           sex ~~ ptsd_child
           sex ~~ ocd_child
           sex ~~ an_child
           sex ~~ adhd_child
           sex ~~ asd_child
           sex ~~ scz_child
           sex ~~ bip_child
           sex ~~ mdd_child
           sex ~~ ts_child
           adhd_child ~~ asd_child
           adhd_child ~~ scz_child
           adhd_child ~~ bip_child
           adhd_child ~~ mdd_child
           adhd_child ~~ anx_child
           adhd_child ~~ alc_child
           adhd_child ~~ ptsd_child
           adhd_child ~~ ocd_child
           adhd_child ~~ an_child
           adhd_child ~~ ts_child
           asd_child ~~ scz_child
           asd_child ~~ bip_child
           asd_child ~~ mdd_child
           asd_child ~~ anx_child
           asd_child ~~ alc_child
           asd_child ~~ ptsd_child
           asd_child ~~ ocd_child
           asd_child ~~ an_child
           asd_child ~~ ts_child
           scz_child ~~ bip_child
           scz_child ~~ mdd_child
           scz_child ~~ anx_child
           scz_child ~~ alc_child
           scz_child ~~ ptsd_child
           scz_child ~~ ocd_child
           scz_child ~~ an_child
           scz_child ~~ ts_child
           bip_child ~~ mdd_child
           bip_child ~~ anx_child
           bip_child ~~ alc_child
           bip_child ~~ ptsd_child
           bip_child ~~ ocd_child
           bip_child ~~ an_child
           bip_child ~~ ts_child
           mdd_child ~~ anx_child
           mdd_child ~~ alc_child
           mdd_child ~~ ptsd_child
           mdd_child ~~ ocd_child
           mdd_child ~~ an_child
           mdd_child ~~ ts_child
           anx_child ~~ alc_child
           anx_child ~~ ptsd_child
           anx_child ~~ ocd_child
           anx_child ~~ an_child
           anx_child ~~ ts_child
           alc_child ~~ ptsd_child
           alc_child ~~ ocd_child
           alc_child ~~ an_child
           alc_child ~~ ts_child
           ptsd_child ~~ ocd_child
           ptsd_child ~~ an_child
           ptsd_child ~~ ts_child
           ocd_child ~~ an_child
           ocd_child ~~ ts_child
           an_child ~~ ts_child
           '
  
  