# 06.1_specify_LGMs_with_trio_pgs.R


# multivariate trio PGS LGM

triomodel <-  
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

          i1 + s1 + i2 + s2 ~ sex

# time-invariant covariate (intercept & variance)

          sex ~~ sex
          sex ~ 1

# regressions

          i1 + s1 ~ adhd_child + adhd_mother + adhd_father
          i1 + s1 ~ asd_child + asd_mother + asd_father
          i1 + s1 ~ scz_child + scz_mother + scz_father
          i1 + s1 ~ bip_child + bip_mother + bip_father
          i1 + s1 ~ mdd_child + mdd_mother + mdd_father
          i1 + s1 ~ anx_child + anx_mother + anx_father
          i1 + s1 ~ alc_child + alc_mother + alc_father
          i1 + s1 ~ ptsd_child + ptsd_mother + ptsd_father
          i1 + s1 ~ ocd_child + ocd_mother + ocd_father
          i1 + s1 ~ an_child + an_mother + an_father
          i1 + s1 ~ ts_child + ts_mother + ts_father
          
          i2 + s2 ~ adhd_child + adhd_mother + adhd_father
          i2 + s2 ~ asd_child + asd_mother + asd_father
          i2 + s2 ~ scz_child + scz_mother + scz_father
          i2 + s2 ~ bip_child + bip_mother + bip_father
          i2 + s2 ~ mdd_child + mdd_mother + mdd_father
          i2 + s2 ~ anx_child + anx_mother + anx_father
          i2 + s2 ~ alc_child + alc_mother + alc_father
          i2 + s2 ~ ptsd_child + ptsd_mother + ptsd_father
          i2 + s2 ~ ocd_child + ocd_mother + ocd_father
          i2 + s2 ~ an_child + an_mother + an_father
          i2 + s2 ~ ts_child + ts_mother + ts_father
                   
# predictors (variance)

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
          
          adhd_mother ~~ adhd_mother
          asd_mother ~~ asd_mother
          scz_mother ~~ scz_mother
          bip_mother ~~ bip_mother
          mdd_mother ~~ mdd_mother
          anx_mother ~~ anx_mother
          alc_mother ~~ alc_mother
          ptsd_mother ~~ ptsd_mother
          ocd_mother ~~ ocd_mother
          an_mother ~~ an_mother
          ts_mother ~~ ts_mother
          
          adhd_father ~~ adhd_father
          asd_father ~~ asd_father
          scz_father ~~ scz_father
          bip_father ~~ bip_father
          mdd_father ~~ mdd_father
          anx_father ~~ anx_father
          alc_father ~~ alc_father
          ptsd_father ~~ ptsd_father
          ocd_father ~~ ocd_father
          an_father ~~ an_father
          ts_father ~~ ts_father
          
# predictors (intercept)
          
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
          
          adhd_mother ~ 1
          asd_mother ~ 1
          scz_mother ~ 1
          bip_mother ~ 1
          mdd_mother ~ 1
          anx_mother ~ 1
          alc_mother ~ 1
          ptsd_mother ~ 1
          ocd_mother ~ 1
          an_mother ~ 1
          ts_mother ~ 1
          
          adhd_father ~ 1
          asd_father ~ 1
          scz_father ~ 1
          bip_father ~ 1
          mdd_father ~ 1
          anx_father ~ 1
          alc_father ~ 1
          ptsd_father ~ 1
          ocd_father ~ 1
          an_father ~ 1
          ts_father ~ 1

# predictors (covariances)

          sex ~~ adhd_child
          sex ~~ asd_child
          sex ~~ scz_child
          sex ~~ bip_child
          sex ~~ mdd_child
          sex ~~ anx_child
          sex ~~ alc_child
          sex ~~ ptsd_child
          sex ~~ ocd_child
          sex ~~ an_child
          sex ~~ ts_child
          sex ~~ adhd_mother
          sex ~~ asd_mother
          sex ~~ scz_mother
          sex ~~ bip_mother
          sex ~~ mdd_mother
          sex ~~ anx_mother
          sex ~~ alc_mother
          sex ~~ ptsd_mother
          sex ~~ ocd_mother
          sex ~~ an_mother
          sex ~~ ts_mother
          sex ~~ adhd_father
          sex ~~ asd_father
          sex ~~ scz_father
          sex ~~ bip_father
          sex ~~ mdd_father
          sex ~~ anx_father
          sex ~~ alc_father
          sex ~~ ptsd_father
          sex ~~ ocd_father
          sex ~~ an_father
          sex ~~ ts_father
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
          
          adhd_mother ~~ asd_mother
          adhd_mother ~~ scz_mother
          adhd_mother ~~ bip_mother
          adhd_mother ~~ mdd_mother
          adhd_mother ~~ anx_mother
          adhd_mother ~~ alc_mother
          adhd_mother ~~ ptsd_mother
          adhd_mother ~~ ocd_mother
          adhd_mother ~~ an_mother
          adhd_mother ~~ ts_mother
          asd_mother ~~ scz_mother
          asd_mother ~~ bip_mother
          asd_mother ~~ mdd_mother
          asd_mother ~~ anx_mother
          asd_mother ~~ alc_mother
          asd_mother ~~ ptsd_mother
          asd_mother ~~ ocd_mother
          asd_mother ~~ an_mother
          asd_mother ~~ ts_mother
          scz_mother ~~ bip_mother
          scz_mother ~~ mdd_mother
          scz_mother ~~ anx_mother
          scz_mother ~~ alc_mother
          scz_mother ~~ ptsd_mother
          scz_mother ~~ ocd_mother
          scz_mother ~~ an_mother
          scz_mother ~~ ts_mother
          bip_mother ~~ mdd_mother
          bip_mother ~~ anx_mother
          bip_mother ~~ alc_mother
          bip_mother ~~ ptsd_mother
          bip_mother ~~ ocd_mother
          bip_mother ~~ an_mother
          bip_mother ~~ ts_mother
          mdd_mother ~~ anx_mother
          mdd_mother ~~ alc_mother
          mdd_mother ~~ ptsd_mother
          mdd_mother ~~ ocd_mother
          mdd_mother ~~ an_mother
          mdd_mother ~~ ts_mother
          anx_mother ~~ alc_mother
          anx_mother ~~ ptsd_mother
          anx_mother ~~ ocd_mother
          anx_mother ~~ an_mother
          anx_mother ~~ ts_mother
          alc_mother ~~ ptsd_mother
          alc_mother ~~ ocd_mother
          alc_mother ~~ an_mother
          alc_mother ~~ ts_mother
          ptsd_mother ~~ ocd_mother
          ptsd_mother ~~ an_mother
          ptsd_mother ~~ ts_mother
          ocd_mother ~~ an_mother
          ocd_mother ~~ ts_mother
          an_mother ~~ ts_mother
          
          adhd_father ~~ asd_father
          adhd_father ~~ scz_father
          adhd_father ~~ bip_father
          adhd_father ~~ mdd_father
          adhd_father ~~ anx_father
          adhd_father ~~ alc_father
          adhd_father ~~ ptsd_father
          adhd_father ~~ ocd_father
          adhd_father ~~ an_father
          adhd_father ~~ ts_father
          asd_father ~~ scz_father
          asd_father ~~ bip_father
          asd_father ~~ mdd_father
          asd_father ~~ anx_father
          asd_father ~~ alc_father
          asd_father ~~ ptsd_father
          asd_father ~~ ocd_father
          asd_father ~~ an_father
          asd_father ~~ ts_father
          scz_father ~~ bip_father
          scz_father ~~ mdd_father
          scz_father ~~ anx_father
          scz_father ~~ alc_father
          scz_father ~~ ptsd_father
          scz_father ~~ ocd_father
          scz_father ~~ an_father
          scz_father ~~ ts_father
          bip_father ~~ mdd_father
          bip_father ~~ anx_father
          bip_father ~~ alc_father
          bip_father ~~ ptsd_father
          bip_father ~~ ocd_father
          bip_father ~~ an_father
          bip_father ~~ ts_father
          mdd_father ~~ anx_father
          mdd_father ~~ alc_father
          mdd_father ~~ ptsd_father
          mdd_father ~~ ocd_father
          mdd_father ~~ an_father
          mdd_father ~~ ts_father
          anx_father ~~ alc_father
          anx_father ~~ ptsd_father
          anx_father ~~ ocd_father
          anx_father ~~ an_father
          anx_father ~~ ts_father
          alc_father ~~ ptsd_father
          alc_father ~~ ocd_father
          alc_father ~~ an_father
          alc_father ~~ ts_father
          ptsd_father ~~ ocd_father
          ptsd_father ~~ an_father
          ptsd_father ~~ ts_father
          ocd_father ~~ an_father
          ocd_father ~~ ts_father
          an_father ~~ ts_father
          
          adhd_child ~~ adhd_mother + adhd_father
          adhd_child ~~ asd_mother + asd_father
          adhd_child ~~ scz_mother + scz_father
          adhd_child ~~ bip_mother + bip_father
          adhd_child ~~ mdd_mother + mdd_father
          adhd_child ~~ anx_mother + anx_father
          adhd_child ~~ alc_mother + alc_father
          adhd_child ~~ ptsd_mother + ptsd_father
          adhd_child ~~ ocd_mother + ocd_father
          adhd_child ~~ an_mother + an_father
          adhd_child ~~ ts_mother + ts_father
          asd_child ~~ adhd_mother + adhd_father
          asd_child ~~ asd_mother + asd_father
          asd_child ~~ scz_mother + scz_father
          asd_child ~~ bip_mother + bip_father
          asd_child ~~ mdd_mother + mdd_father
          asd_child ~~ anx_mother + anx_father
          asd_child ~~ alc_mother + alc_father
          asd_child ~~ ptsd_mother + ptsd_father
          asd_child ~~ ocd_mother + ocd_father
          asd_child ~~ an_mother + an_father
          asd_child ~~ ts_mother + ts_father
          scz_child ~~ adhd_mother + adhd_father
          scz_child ~~ asd_mother + asd_father
          scz_child ~~ scz_mother + scz_father
          scz_child ~~ bip_mother + bip_father
          scz_child ~~ mdd_mother + mdd_father
          scz_child ~~ anx_mother + anx_father
          scz_child ~~ alc_mother + alc_father
          scz_child ~~ ptsd_mother + ptsd_father
          scz_child ~~ ocd_mother + ocd_father
          scz_child ~~ an_mother + an_father
          scz_child ~~ ts_mother + ts_father
          bip_child ~~ adhd_mother + adhd_father
          bip_child ~~ asd_mother + asd_father
          bip_child ~~ scz_mother + scz_father
          bip_child ~~ bip_mother + bip_father
          bip_child ~~ mdd_mother + mdd_father
          bip_child ~~ anx_mother + anx_father
          bip_child ~~ alc_mother + alc_father
          bip_child ~~ ptsd_mother + ptsd_father
          bip_child ~~ ocd_mother + ocd_father
          bip_child ~~ an_mother + an_father
          bip_child ~~ ts_mother + ts_father
          mdd_child ~~ adhd_mother + adhd_father
          mdd_child ~~ asd_mother + asd_father
          mdd_child ~~ scz_mother + scz_father
          mdd_child ~~ bip_mother + bip_father
          mdd_child ~~ mdd_mother + mdd_father
          mdd_child ~~ anx_mother + anx_father
          mdd_child ~~ alc_mother + alc_father
          mdd_child ~~ ptsd_mother + ptsd_father
          mdd_child ~~ ocd_mother + ocd_father
          mdd_child ~~ an_mother + an_father
          mdd_child ~~ ts_mother + ts_father
          anx_child ~~ adhd_mother + adhd_father
          anx_child ~~ asd_mother + asd_father
          anx_child ~~ scz_mother + scz_father
          anx_child ~~ bip_mother + bip_father
          anx_child ~~ mdd_mother + mdd_father
          anx_child ~~ anx_mother + anx_father
          anx_child ~~ alc_mother + alc_father
          anx_child ~~ ptsd_mother + ptsd_father
          anx_child ~~ ocd_mother + ocd_father
          anx_child ~~ an_mother + an_father
          anx_child ~~ ts_mother + ts_father
          alc_child ~~ adhd_mother + adhd_father
          alc_child ~~ asd_mother + asd_father
          alc_child ~~ scz_mother + scz_father
          alc_child ~~ bip_mother + bip_father
          alc_child ~~ mdd_mother + mdd_father
          alc_child ~~ anx_mother + anx_father
          alc_child ~~ alc_mother + alc_father
          alc_child ~~ ptsd_mother + ptsd_father
          alc_child ~~ ocd_mother + ocd_father
          alc_child ~~ an_mother + an_father
          alc_child ~~ ts_mother + ts_father
          ptsd_child ~~ adhd_mother + adhd_father
          ptsd_child ~~ asd_mother + asd_father
          ptsd_child ~~ scz_mother + scz_father
          ptsd_child ~~ bip_mother + bip_father
          ptsd_child ~~ mdd_mother + mdd_father
          ptsd_child ~~ anx_mother + anx_father
          ptsd_child ~~ alc_mother + alc_father
          ptsd_child ~~ ptsd_mother + ptsd_father
          ptsd_child ~~ ocd_mother + ocd_father
          ptsd_child ~~ an_mother + an_father
          ptsd_child ~~ ts_mother + ts_father
          ocd_child ~~ adhd_mother + adhd_father
          ocd_child ~~ asd_mother + asd_father
          ocd_child ~~ scz_mother + scz_father         
          ocd_child ~~ bip_mother + bip_father
          ocd_child ~~ mdd_mother + mdd_father         
          ocd_child ~~ anx_mother + anx_father
          ocd_child ~~ alc_mother + alc_father
          ocd_child ~~ ptsd_mother + ptsd_father
          ocd_child ~~ ocd_mother + ocd_father
          ocd_child ~~ an_mother + an_father
          ocd_child ~~ ts_mother + ts_father
          an_child ~~ adhd_mother + adhd_father
          an_child ~~ asd_mother + asd_father
          an_child ~~ scz_mother + scz_father
          an_child ~~ bip_mother + bip_father
          an_child ~~ mdd_mother + mdd_father
          an_child ~~ anx_mother + anx_father
          an_child ~~ alc_mother + alc_father
          an_child ~~ ptsd_mother + ptsd_father
          an_child ~~ ocd_mother + ocd_father
          an_child ~~ an_mother + an_father
          an_child ~~ ts_mother + ts_father
          ts_child ~~ adhd_mother + adhd_father
          ts_child ~~ asd_mother + asd_father
          ts_child ~~ scz_mother + scz_father
          ts_child ~~ bip_mother + bip_father
          ts_child ~~ mdd_mother + mdd_father
          ts_child ~~ anx_mother + anx_father
          ts_child ~~ alc_mother + alc_father
          ts_child ~~ ptsd_mother + ptsd_father
          ts_child ~~ ocd_mother + ocd_father
          ts_child ~~ an_mother + an_father
          ts_child ~~ ts_mother + ts_father
          
          adhd_mother ~~ adhd_father
          adhd_mother ~~ asd_father
          adhd_mother ~~ scz_father
          adhd_mother ~~ bip_father
          adhd_mother ~~ mdd_father
          adhd_mother ~~ anx_father
          adhd_mother ~~ alc_father
          adhd_mother ~~ ptsd_father
          adhd_mother ~~ ocd_father
          adhd_mother ~~ an_father
          adhd_mother ~~ ts_father
          asd_mother ~~ adhd_father
          asd_mother ~~ asd_father
          asd_mother ~~ scz_father
          asd_mother ~~ bip_father
          asd_mother ~~ mdd_father
          asd_mother ~~ anx_father
          asd_mother ~~ alc_father
          asd_mother ~~ ptsd_father
          asd_mother ~~ ocd_father
          asd_mother ~~ an_father
          asd_mother ~~ ts_father
          scz_mother ~~ adhd_father
          scz_mother ~~ asd_father
          scz_mother ~~ scz_father
          scz_mother ~~ bip_father
          scz_mother ~~ mdd_father
          scz_mother ~~ anx_father
          scz_mother ~~ alc_father
          scz_mother ~~ ptsd_father
          scz_mother ~~ ocd_father
          scz_mother ~~ an_father
          scz_mother ~~ ts_father
          bip_mother ~~ adhd_father
          bip_mother ~~ asd_father
          bip_mother ~~ scz_father
          bip_mother ~~ bip_father
          bip_mother ~~ mdd_father
          bip_mother ~~ anx_father
          bip_mother ~~ alc_father
          bip_mother ~~ ptsd_father
          bip_mother ~~ ocd_father
          bip_mother ~~ an_father
          bip_mother ~~ ts_father
          mdd_mother ~~ adhd_father
          mdd_mother ~~ asd_father
          mdd_mother ~~ scz_father
          mdd_mother ~~ bip_father
          mdd_mother ~~ mdd_father
          mdd_mother ~~ anx_father
          mdd_mother ~~ alc_father
          mdd_mother ~~ ptsd_father
          mdd_mother ~~ ocd_father
          mdd_mother ~~ an_father
          mdd_mother ~~ ts_father
          anx_mother ~~ adhd_father
          anx_mother ~~ asd_father
          anx_mother ~~ scz_father
          anx_mother ~~ bip_father
          anx_mother ~~ mdd_father
          anx_mother ~~ anx_father
          anx_mother ~~ alc_father
          anx_mother ~~ ptsd_father
          anx_mother ~~ ocd_father
          anx_mother ~~ an_father
          anx_mother ~~ ts_father
          alc_mother ~~ adhd_father
          alc_mother ~~ asd_father
          alc_mother ~~ scz_father
          alc_mother ~~ bip_father
          alc_mother ~~ mdd_father
          alc_mother ~~ anx_father
          alc_mother ~~ alc_father
          alc_mother ~~ ptsd_father
          alc_mother ~~ ocd_father
          alc_mother ~~ an_father
          alc_mother ~~ ts_father
          ptsd_mother ~~ adhd_father
          ptsd_mother ~~ asd_father
          ptsd_mother ~~ scz_father
          ptsd_mother ~~ bip_father
          ptsd_mother ~~ mdd_father
          ptsd_mother ~~ anx_father
          ptsd_mother ~~ alc_father
          ptsd_mother ~~ ptsd_father
          ptsd_mother ~~ ocd_father
          ptsd_mother ~~ an_father
          ptsd_mother ~~ ts_father
          ocd_mother ~~ adhd_father
          ocd_mother ~~ asd_father
          ocd_mother ~~ scz_father         
          ocd_mother ~~ bip_father
          ocd_mother ~~ mdd_father         
          ocd_mother ~~ anx_father
          ocd_mother ~~ alc_father
          ocd_mother ~~ ptsd_father
          ocd_mother ~~ ocd_father
          ocd_mother ~~ an_father
          ocd_mother ~~ ts_father
          an_mother ~~ adhd_father
          an_mother ~~ asd_father
          an_mother ~~ scz_father
          an_mother ~~ bip_father
          an_mother ~~ mdd_father
          an_mother ~~ anx_father
          an_mother ~~ alc_father
          an_mother ~~ ptsd_father
          an_mother ~~ ocd_father
          an_mother ~~ an_father
          an_mother ~~ ts_father
          ts_mother ~~ adhd_father
          ts_mother ~~ asd_father
          ts_mother ~~ scz_father
          ts_mother ~~ bip_father
          ts_mother ~~ mdd_father
          ts_mother ~~ anx_father
          ts_mother ~~ alc_father
          ts_mother ~~ ptsd_father
          ts_mother ~~ ocd_father
          ts_mother ~~ an_father
          ts_mother ~~ ts_father
          '

# univariate trio PGS LGMs

uni_triomodel <-  
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

          i1 + s1 + i2 + s2 ~ sex

# time-invariant covariate (intercept & variance)

          sex ~~ sex
          sex ~ 1

# regressions

          i1 + s1 ~ pgs_child + pgs_mother + pgs_father
          i2 + s2 ~ pgs_child + pgs_mother + pgs_father
                   
# predictors (variance)

          pgs_child ~~ pgs_child
          pgs_mother ~~ pgs_mother
          pgs_father ~~ pgs_father
          
# predictors (intercept)
          
          pgs_child ~ 1
          pgs_mother ~ 1
          pgs_father ~ 1

# predictors (covariances)

          sex ~~ pgs_child
          sex ~~ pgs_mother
          sex ~~ pgs_father
          
          pgs_child ~~ pgs_mother + pgs_father
          pgs_mother ~~ pgs_father
          '





