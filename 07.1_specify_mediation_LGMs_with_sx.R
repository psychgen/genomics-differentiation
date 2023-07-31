#07.1_specify_mediation_LGMs_with_sx.R


# differentiation mediating relationship between PGS and symptoms

 model1 <- ' 
           # growth parameters (latent variables)
  
             i =~ 1*diff1 + 1*diff2 + 1*diff3
             s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
           # growth parameter (co)variances
    
             i ~~ i
             s ~~ s
             i ~~ s
             
           # obs variable intercepts (fixed to 0)
  
             diff1 ~ 0*1
             diff2 ~ 0*1
             diff3 ~ 0*1
           
           # growth parameter intercepts (freely estimated)
  
             i ~ 1 
             s ~ 1
             
           # obs variable variances

             diff1 ~~ diff1
             diff2 ~~ diff2
             diff3 ~~ diff3
             dep_8yr ~~ dep_8yr
             anx_8yr ~~ anx_8yr
             inat_8yr ~~ inat_8yr
             hyp_8yr ~~ hyp_8yr
             cd_8yr ~~ cd_8yr
             odd_8yr ~~ odd_8yr
  
           # means of outcomes (freely estimated)
  
             dep_8yr ~ 1
             anx_8yr ~ 1
             inat_8yr ~ 1
             hyp_8yr ~ 1
             cd_8yr ~ 1
             odd_8yr ~ 1
  
           # covariances of outcomes (freely estimated)
  
             dep_8yr ~~ anx_8yr + inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             anx_8yr ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr
             hyp_8yr ~~ cd_8yr + odd_8yr
             cd_8yr ~~ odd_8yr
             
           # covariances of difference scores with outcomes (fixed to zero)
  
             diff1 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             diff2 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             diff3 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             
           # time-invariant covariate (effects)

             i + s ~ sex

           # time-invariant covariate (intercept & variance)

             sex ~~ sex
             sex ~ 1
          
           # time-invariant covariate (covariances with outcomes)

             sex ~~ dep_8yr + anx_8yr + inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             
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

           # direct effects: c
 
             dep_8yr ~ c_dep_adhd*adhd_child+c_dep_asd*asd_child+c_dep_scz*scz_child+c_dep_bip*bip_child+c_dep_mdd*mdd_child+c_dep_anx*anx_child+c_dep_alc*alc_child+c_dep_ptsd*ptsd_child+c_dep_ocd*ocd_child+c_dep_an*an_child+c_dep_ts*ts_child
             anx_8yr ~ c_anx_adhd*adhd_child+c_anx_asd*asd_child+c_anx_scz*scz_child+c_anx_bip*bip_child+c_anx_mdd*mdd_child+c_anx_anx*anx_child+c_anx_alc*alc_child+c_anx_ptsd*ptsd_child+c_anx_ocd*ocd_child+c_anx_an*an_child+c_anx_ts*ts_child
             inat_8yr ~ c_inat_adhd*adhd_child+c_inat_asd*asd_child+c_inat_scz*scz_child+c_inat_bip*bip_child+c_inat_mdd*mdd_child+c_inat_anx*anx_child+c_inat_alc*alc_child+c_inat_ptsd*ptsd_child+c_inat_ocd*ocd_child+c_inat_an*an_child+c_inat_ts*ts_child
             hyp_8yr ~ c_hyp_adhd*adhd_child+c_hyp_asd*asd_child+c_hyp_scz*scz_child+c_hyp_bip*bip_child+c_hyp_mdd*mdd_child+c_hyp_anx*anx_child+c_hyp_alc*alc_child+c_hyp_ptsd*ptsd_child+c_hyp_ocd*ocd_child+c_hyp_an*an_child+c_hyp_ts*ts_child
             cd_8yr ~ c_cd_adhd*adhd_child+c_cd_asd*asd_child+c_cd_scz*scz_child+c_cd_bip*bip_child+c_cd_mdd*mdd_child+c_cd_anx*anx_child+c_cd_alc*alc_child+c_cd_ptsd*ptsd_child+c_cd_ocd*ocd_child+c_cd_an*an_child+c_cd_ts*ts_child
             odd_8yr ~ c_odd_adhd*adhd_child+c_odd_asd*asd_child+c_odd_scz*scz_child+c_odd_bip*bip_child+c_odd_mdd*mdd_child+c_odd_anx*anx_child+c_odd_alc*alc_child+c_odd_ptsd*ptsd_child+c_odd_ocd*ocd_child+c_odd_an*an_child+c_odd_ts*ts_child
             
           # mediation effects: a & b paths
           
             i ~ a_i_adhd*adhd_child+a_i_asd*asd_child+a_i_scz*scz_child+a_i_bip*bip_child+a_i_mdd*mdd_child+a_i_anx*anx_child+a_i_alc*alc_child+a_i_ptsd*ptsd_child+a_i_ocd*ocd_child+a_i_an*an_child+a_i_ts*ts_child
             s ~ a_s_adhd*adhd_child+a_s_asd*asd_child+a_s_scz*scz_child+a_s_bip*bip_child+a_s_mdd*mdd_child+a_s_anx*anx_child+a_s_alc*alc_child+a_s_ptsd*ptsd_child+a_s_ocd*ocd_child+a_s_an*an_child+a_s_ts*ts_child
             dep_8yr ~ b_i_dep*i + b_s_dep*s
             anx_8yr ~ b_i_anx*i + b_s_anx*s
             inat_8yr ~ b_i_inat*i + b_s_inat*s
             hyp_8yr ~ b_i_hyp*i + b_s_hyp*s
             cd_8yr ~ b_i_cd*i + b_s_cd*s
             odd_8yr ~ b_i_odd*i + b_s_odd*s
             
           # indirect effects: a*b
           
             ab_i_dep_adhd := a_i_adhd*b_i_dep
             ab_s_dep_adhd := a_s_adhd*b_s_dep
             ab_i_anx_adhd := a_i_adhd*b_i_anx
             ab_s_anx_adhd := a_s_adhd*b_s_anx
             ab_i_inat_adhd := a_i_adhd*b_i_inat
             ab_s_inat_adhd := a_s_adhd*b_s_inat
             ab_i_hyp_adhd := a_i_adhd*b_i_hyp
             ab_s_hyp_adhd := a_s_adhd*b_s_hyp
             ab_i_cd_adhd := a_i_adhd*b_i_cd
             ab_s_cd_adhd := a_s_adhd*b_s_cd
             ab_i_odd_adhd := a_i_adhd*b_i_odd
             ab_s_odd_adhd := a_s_adhd*b_s_odd
             
             ab_i_dep_asd := a_i_asd*b_i_dep
             ab_s_dep_asd := a_s_asd*b_s_dep
             ab_i_anx_asd := a_i_asd*b_i_anx
             ab_s_anx_asd := a_s_asd*b_s_anx
             ab_i_inat_asd := a_i_asd*b_i_inat
             ab_s_inat_asd := a_s_asd*b_s_inat
             ab_i_hyp_asd := a_i_asd*b_i_hyp
             ab_s_hyp_asd := a_s_asd*b_s_hyp
             ab_i_cd_asd := a_i_asd*b_i_cd
             ab_s_cd_asd := a_s_asd*b_s_cd
             ab_i_odd_asd := a_i_asd*b_i_odd
             ab_s_odd_asd := a_s_asd*b_s_odd
             
             ab_i_dep_scz := a_i_scz*b_i_dep
             ab_s_dep_scz := a_s_scz*b_s_dep
             ab_i_anx_scz := a_i_scz*b_i_anx
             ab_s_anx_scz := a_s_scz*b_s_anx
             ab_i_inat_scz := a_i_scz*b_i_inat
             ab_s_inat_scz := a_s_scz*b_s_inat
             ab_i_hyp_scz := a_i_scz*b_i_hyp
             ab_s_hyp_scz := a_s_scz*b_s_hyp
             ab_i_cd_scz := a_i_scz*b_i_cd
             ab_s_cd_scz := a_s_scz*b_s_cd
             ab_i_odd_scz := a_i_scz*b_i_odd
             ab_s_odd_scz := a_s_scz*b_s_odd
             
             ab_i_dep_bip := a_i_bip*b_i_dep
             ab_s_dep_bip := a_s_bip*b_s_dep
             ab_i_anx_bip := a_i_bip*b_i_anx
             ab_s_anx_bip := a_s_bip*b_s_anx
             ab_i_inat_bip := a_i_bip*b_i_inat
             ab_s_inat_bip := a_s_bip*b_s_inat
             ab_i_hyp_bip := a_i_bip*b_i_hyp
             ab_s_hyp_bip := a_s_bip*b_s_hyp
             ab_i_cd_bip := a_i_bip*b_i_cd
             ab_s_cd_bip := a_s_bip*b_s_cd
             ab_i_odd_bip := a_i_bip*b_i_odd
             ab_s_odd_bip := a_s_bip*b_s_odd
             
             ab_i_dep_mdd := a_i_mdd*b_i_dep
             ab_s_dep_mdd := a_s_mdd*b_s_dep
             ab_i_anx_mdd := a_i_mdd*b_i_anx
             ab_s_anx_mdd := a_s_mdd*b_s_anx
             ab_i_inat_mdd := a_i_mdd*b_i_inat
             ab_s_inat_mdd := a_s_mdd*b_s_inat
             ab_i_hyp_mdd := a_i_mdd*b_i_hyp
             ab_s_hyp_mdd := a_s_mdd*b_s_hyp
             ab_i_cd_mdd := a_i_mdd*b_i_cd
             ab_s_cd_mdd := a_s_mdd*b_s_cd
             ab_i_odd_mdd := a_i_mdd*b_i_odd
             ab_s_odd_mdd := a_s_mdd*b_s_odd
             
             ab_i_dep_anx := a_i_anx*b_i_dep
             ab_s_dep_anx := a_s_anx*b_s_dep
             ab_i_anx_anx := a_i_anx*b_i_anx
             ab_s_anx_anx := a_s_anx*b_s_anx
             ab_i_inat_anx := a_i_anx*b_i_inat
             ab_s_inat_anx := a_s_anx*b_s_inat
             ab_i_hyp_anx := a_i_anx*b_i_hyp
             ab_s_hyp_anx := a_s_anx*b_s_hyp
             ab_i_cd_anx := a_i_anx*b_i_cd
             ab_s_cd_anx := a_s_anx*b_s_cd
             ab_i_odd_anx := a_i_anx*b_i_odd
             ab_s_odd_anx := a_s_anx*b_s_odd
             
             ab_i_dep_alc := a_i_alc*b_i_dep
             ab_s_dep_alc := a_s_alc*b_s_dep
             ab_i_anx_alc := a_i_alc*b_i_anx
             ab_s_anx_alc := a_s_alc*b_s_anx
             ab_i_inat_alc := a_i_alc*b_i_inat
             ab_s_inat_alc := a_s_alc*b_s_inat
             ab_i_hyp_alc := a_i_alc*b_i_hyp
             ab_s_hyp_alc := a_s_alc*b_s_hyp
             ab_i_cd_alc := a_i_alc*b_i_cd
             ab_s_cd_alc := a_s_alc*b_s_cd
             ab_i_odd_alc := a_i_alc*b_i_odd
             ab_s_odd_alc := a_s_alc*b_s_odd
             
             ab_i_dep_ptsd := a_i_ptsd*b_i_dep
             ab_s_dep_ptsd := a_s_ptsd*b_s_dep
             ab_i_anx_ptsd := a_i_ptsd*b_i_anx
             ab_s_anx_ptsd := a_s_ptsd*b_s_anx
             ab_i_inat_ptsd := a_i_ptsd*b_i_inat
             ab_s_inat_ptsd := a_s_ptsd*b_s_inat
             ab_i_hyp_ptsd := a_i_ptsd*b_i_hyp
             ab_s_hyp_ptsd := a_s_ptsd*b_s_hyp
             ab_i_cd_ptsd := a_i_ptsd*b_i_cd
             ab_s_cd_ptsd := a_s_ptsd*b_s_cd
             ab_i_odd_ptsd := a_i_ptsd*b_i_odd
             ab_s_odd_ptsd := a_s_ptsd*b_s_odd
             
             ab_i_dep_ocd := a_i_ocd*b_i_dep
             ab_s_dep_ocd := a_s_ocd*b_s_dep
             ab_i_anx_ocd := a_i_ocd*b_i_anx
             ab_s_anx_ocd := a_s_ocd*b_s_anx
             ab_i_inat_ocd := a_i_ocd*b_i_inat
             ab_s_inat_ocd := a_s_ocd*b_s_inat
             ab_i_hyp_ocd := a_i_ocd*b_i_hyp
             ab_s_hyp_ocd := a_s_ocd*b_s_hyp
             ab_i_cd_ocd := a_i_ocd*b_i_cd
             ab_s_cd_ocd := a_s_ocd*b_s_cd
             ab_i_odd_ocd := a_i_ocd*b_i_odd
             ab_s_odd_ocd := a_s_ocd*b_s_odd
             
             ab_i_dep_an := a_i_an*b_i_dep
             ab_s_dep_an := a_s_an*b_s_dep
             ab_i_anx_an := a_i_an*b_i_anx
             ab_s_anx_an := a_s_an*b_s_anx
             ab_i_inat_an := a_i_an*b_i_inat
             ab_s_inat_an := a_s_an*b_s_inat
             ab_i_hyp_an := a_i_an*b_i_hyp
             ab_s_hyp_an := a_s_an*b_s_hyp
             ab_i_cd_an := a_i_an*b_i_cd
             ab_s_cd_an := a_s_an*b_s_cd
             ab_i_odd_an := a_i_an*b_i_odd
             ab_s_odd_an := a_s_an*b_s_odd
             
             ab_i_dep_ts := a_i_ts*b_i_dep
             ab_s_dep_ts := a_s_ts*b_s_dep
             ab_i_anx_ts := a_i_ts*b_i_anx
             ab_s_anx_ts := a_s_ts*b_s_anx
             ab_i_inat_ts := a_i_ts*b_i_inat
             ab_s_inat_ts := a_s_ts*b_s_inat
             ab_i_hyp_ts := a_i_ts*b_i_hyp
             ab_s_hyp_ts := a_s_ts*b_s_hyp
             ab_i_cd_ts := a_i_ts*b_i_cd
             ab_s_cd_ts := a_s_ts*b_s_cd
             ab_i_odd_ts := a_i_ts*b_i_odd
             ab_s_odd_ts := a_s_ts*b_s_odd
             
           # total effects: total := c + (a*b)
             
             total_i_dep_adhd := c_dep_adhd + (a_i_adhd*b_i_dep)
             total_s_dep_adhd := c_dep_adhd + (a_s_adhd*b_s_dep)
             total_i_anx_adhd := c_anx_adhd + (a_i_adhd*b_i_anx)
             total_s_anx_adhd := c_anx_adhd + (a_s_adhd*b_s_anx)
             total_i_inat_adhd := c_inat_adhd + (a_i_adhd*b_i_inat)
             total_s_inat_adhd := c_inat_adhd + (a_s_adhd*b_s_inat)
             total_i_hyp_adhd := c_hyp_adhd + (a_i_adhd*b_i_hyp)
             total_s_hyp_adhd := c_hyp_adhd + (a_s_adhd*b_s_hyp)
             total_i_cd_adhd := c_cd_adhd + (a_i_adhd*b_i_cd)
             total_s_cd_adhd := c_cd_adhd + (a_s_adhd*b_s_cd)
             total_i_odd_adhd := c_odd_adhd + (a_i_adhd*b_i_odd)
             total_s_odd_adhd := c_odd_adhd + (a_s_adhd*b_s_odd)
             
             total_i_dep_asd := c_dep_asd + (a_i_asd*b_i_dep)
             total_s_dep_asd := c_dep_asd + (a_s_asd*b_s_dep)
             total_i_anx_asd := c_anx_asd + (a_i_asd*b_i_anx)
             total_s_anx_asd := c_anx_asd + (a_s_asd*b_s_anx)
             total_i_inat_asd := c_inat_asd + (a_i_asd*b_i_inat)
             total_s_inat_asd := c_inat_asd + (a_s_asd*b_s_inat)
             total_i_hyp_asd := c_hyp_asd + (a_i_asd*b_i_hyp)
             total_s_hyp_asd := c_hyp_asd + (a_s_asd*b_s_hyp)
             total_i_cd_asd := c_cd_asd + (a_i_asd*b_i_cd)
             total_s_cd_asd := c_cd_asd + (a_s_asd*b_s_cd)
             total_i_odd_asd := c_odd_asd + (a_i_asd*b_i_odd)
             total_s_odd_asd := c_odd_asd + (a_s_asd*b_s_odd)
             
             total_i_dep_scz := c_dep_scz + (a_i_scz*b_i_dep)
             total_s_dep_scz := c_dep_scz + (a_s_scz*b_s_dep)
             total_i_anx_scz := c_anx_scz + (a_i_scz*b_i_anx)
             total_s_anx_scz := c_anx_scz + (a_s_scz*b_s_anx)
             total_i_inat_scz := c_inat_scz + (a_i_scz*b_i_inat)
             total_s_inat_scz := c_inat_scz + (a_s_scz*b_s_inat)
             total_i_hyp_scz := c_hyp_scz + (a_i_scz*b_i_hyp)
             total_s_hyp_scz := c_hyp_scz + (a_s_scz*b_s_hyp)
             total_i_cd_scz := c_cd_scz + (a_i_scz*b_i_cd)
             total_s_cd_scz := c_cd_scz + (a_s_scz*b_s_cd)
             total_i_odd_scz := c_odd_scz + (a_i_scz*b_i_odd)
             total_s_odd_scz := c_odd_scz + (a_s_scz*b_s_odd)
             
             total_i_dep_bip := c_dep_bip + (a_i_bip*b_i_dep)
             total_s_dep_bip := c_dep_bip + (a_s_bip*b_s_dep)
             total_i_anx_bip := c_anx_bip + (a_i_bip*b_i_anx)
             total_s_anx_bip := c_anx_bip + (a_s_bip*b_s_anx)
             total_i_inat_bip := c_inat_bip + (a_i_bip*b_i_inat)
             total_s_inat_bip := c_inat_bip + (a_s_bip*b_s_inat)
             total_i_hyp_bip := c_hyp_bip + (a_i_bip*b_i_hyp)
             total_s_hyp_bip := c_hyp_bip + (a_s_bip*b_s_hyp)
             total_i_cd_bip := c_cd_bip + (a_i_bip*b_i_cd)
             total_s_cd_bip := c_cd_bip + (a_s_bip*b_s_cd)
             total_i_odd_bip := c_odd_bip + (a_i_bip*b_i_odd)
             total_s_odd_bip := c_odd_bip + (a_s_bip*b_s_odd)
             
             total_i_dep_mdd := c_dep_mdd + (a_i_mdd*b_i_dep)
             total_s_dep_mdd := c_dep_mdd + (a_s_mdd*b_s_dep)
             total_i_anx_mdd := c_anx_mdd + (a_i_mdd*b_i_anx)
             total_s_anx_mdd := c_anx_mdd + (a_s_mdd*b_s_anx)
             total_i_inat_mdd := c_inat_mdd + (a_i_mdd*b_i_inat)
             total_s_inat_mdd := c_inat_mdd + (a_s_mdd*b_s_inat)
             total_i_hyp_mdd := c_hyp_mdd + (a_i_mdd*b_i_hyp)
             total_s_hyp_mdd := c_hyp_mdd + (a_s_mdd*b_s_hyp)
             total_i_cd_mdd := c_cd_mdd + (a_i_mdd*b_i_cd)
             total_s_cd_mdd := c_cd_mdd + (a_s_mdd*b_s_cd)
             total_i_odd_mdd := c_odd_mdd + (a_i_mdd*b_i_odd)
             total_s_odd_mdd := c_odd_mdd + (a_s_mdd*b_s_odd)
             
             total_i_dep_anx := c_dep_anx + (a_i_anx*b_i_dep)
             total_s_dep_anx := c_dep_anx + (a_s_anx*b_s_dep)
             total_i_anx_anx := c_anx_anx + (a_i_anx*b_i_anx)
             total_s_anx_anx := c_anx_anx + (a_s_anx*b_s_anx)
             total_i_inat_anx := c_inat_anx + (a_i_anx*b_i_inat)
             total_s_inat_anx := c_inat_anx + (a_s_anx*b_s_inat)
             total_i_hyp_anx := c_hyp_anx + (a_i_anx*b_i_hyp)
             total_s_hyp_anx := c_hyp_anx + (a_s_anx*b_s_hyp)
             total_i_cd_anx := c_cd_anx + (a_i_anx*b_i_cd)
             total_s_cd_anx := c_cd_anx + (a_s_anx*b_s_cd)
             total_i_odd_anx := c_odd_anx + (a_i_anx*b_i_odd)
             total_s_odd_anx := c_odd_anx + (a_s_anx*b_s_odd)
             
             total_i_dep_alc := c_dep_alc + (a_i_alc*b_i_dep)
             total_s_dep_alc := c_dep_alc + (a_s_alc*b_s_dep)
             total_i_anx_alc := c_anx_alc + (a_i_alc*b_i_anx)
             total_s_anx_alc := c_anx_alc + (a_s_alc*b_s_anx)
             total_i_inat_alc := c_inat_alc + (a_i_alc*b_i_inat)
             total_s_inat_alc := c_inat_alc + (a_s_alc*b_s_inat)
             total_i_hyp_alc := c_hyp_alc + (a_i_alc*b_i_hyp)
             total_s_hyp_alc := c_hyp_alc + (a_s_alc*b_s_hyp)
             total_i_cd_alc := c_cd_alc + (a_i_alc*b_i_cd)
             total_s_cd_alc := c_cd_alc + (a_s_alc*b_s_cd)
             total_i_odd_alc := c_odd_alc + (a_i_alc*b_i_odd)
             total_s_odd_alc := c_odd_alc + (a_s_alc*b_s_odd)
             
             total_i_dep_ptsd := c_dep_ptsd + (a_i_ptsd*b_i_dep)
             total_s_dep_ptsd := c_dep_ptsd + (a_s_ptsd*b_s_dep)
             total_i_anx_ptsd := c_anx_ptsd + (a_i_ptsd*b_i_anx)
             total_s_anx_ptsd := c_anx_ptsd + (a_s_ptsd*b_s_anx)
             total_i_inat_ptsd := c_inat_ptsd + (a_i_ptsd*b_i_inat)
             total_s_inat_ptsd := c_inat_ptsd + (a_s_ptsd*b_s_inat)
             total_i_hyp_ptsd := c_hyp_ptsd + (a_i_ptsd*b_i_hyp)
             total_s_hyp_ptsd := c_hyp_ptsd + (a_s_ptsd*b_s_hyp)
             total_i_cd_ptsd := c_cd_ptsd + (a_i_ptsd*b_i_cd)
             total_s_cd_ptsd := c_cd_ptsd + (a_s_ptsd*b_s_cd)
             total_i_odd_ptsd := c_odd_ptsd + (a_i_ptsd*b_i_odd)
             total_s_odd_ptsd := c_odd_ptsd + (a_s_ptsd*b_s_odd)
             
             total_i_dep_ocd := c_dep_ocd + (a_i_ocd*b_i_dep)
             total_s_dep_ocd := c_dep_ocd + (a_s_ocd*b_s_dep)
             total_i_anx_ocd := c_anx_ocd + (a_i_ocd*b_i_anx)
             total_s_anx_ocd := c_anx_ocd + (a_s_ocd*b_s_anx)
             total_i_inat_ocd := c_inat_ocd + (a_i_ocd*b_i_inat)
             total_s_inat_ocd := c_inat_ocd + (a_s_ocd*b_s_inat)
             total_i_hyp_ocd := c_hyp_ocd + (a_i_ocd*b_i_hyp)
             total_s_hyp_ocd := c_hyp_ocd + (a_s_ocd*b_s_hyp)
             total_i_cd_ocd := c_cd_ocd + (a_i_ocd*b_i_cd)
             total_s_cd_ocd := c_cd_ocd + (a_s_ocd*b_s_cd)
             total_i_odd_ocd := c_odd_ocd + (a_i_ocd*b_i_odd)
             total_s_odd_ocd := c_odd_ocd + (a_s_ocd*b_s_odd)
             
             total_i_dep_an := c_dep_an + (a_i_an*b_i_dep)
             total_s_dep_an := c_dep_an + (a_s_an*b_s_dep)
             total_i_anx_an := c_anx_an + (a_i_an*b_i_anx)
             total_s_anx_an := c_anx_an + (a_s_an*b_s_anx)
             total_i_inat_an := c_inat_an + (a_i_an*b_i_inat)
             total_s_inat_an := c_inat_an + (a_s_an*b_s_inat)
             total_i_hyp_an := c_hyp_an + (a_i_an*b_i_hyp)
             total_s_hyp_an := c_hyp_an + (a_s_an*b_s_hyp)
             total_i_cd_an := c_cd_an + (a_i_an*b_i_cd)
             total_s_cd_an := c_cd_an + (a_s_an*b_s_cd)
             total_i_odd_an := c_odd_an + (a_i_an*b_i_odd)
             total_s_odd_an := c_odd_an + (a_s_an*b_s_odd)
             
             total_i_dep_ts := c_dep_ts + (a_i_ts*b_i_dep)
             total_s_dep_ts := c_dep_ts + (a_s_ts*b_s_dep)
             total_i_anx_ts := c_anx_ts + (a_i_ts*b_i_anx)
             total_s_anx_ts := c_anx_ts + (a_s_ts*b_s_anx)
             total_i_inat_ts := c_inat_ts + (a_i_ts*b_i_inat)
             total_s_inat_ts := c_inat_ts + (a_s_ts*b_s_inat)
             total_i_hyp_ts := c_hyp_ts + (a_i_ts*b_i_hyp)
             total_s_hyp_ts := c_hyp_ts + (a_s_ts*b_s_hyp)
             total_i_cd_ts := c_cd_ts + (a_i_ts*b_i_cd)
             total_s_cd_ts := c_cd_ts + (a_s_ts*b_s_cd)
             total_i_odd_ts := c_odd_ts + (a_i_ts*b_i_odd)
             total_s_odd_ts := c_odd_ts + (a_s_ts*b_s_odd)
             
             '
 
# total problems mediating relationship between PGS and symptoms
 
 model2 <- ' 
           # growth parameters (latent variables)
  
             i =~ 1*tot1 + 1*tot2 + 1*tot3
             s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
           # growth parameter (co)variances
    
             i ~~ i
             s ~~ s
             i ~~ s
             
           # obs variable intercepts (fixed to 0)
  
             tot1 ~ 0*1
             tot2 ~ 0*1
             tot3 ~ 0*1
           
           # growth parameter intercepts (freely estimated)
  
             i ~ 1 
             s ~ 1
             
           # obs variable variances

             tot1 ~~ tot1
             tot2 ~~ tot2
             tot3 ~~ tot3
             dep_8yr ~~ dep_8yr
             anx_8yr ~~ anx_8yr
             inat_8yr ~~ inat_8yr
             hyp_8yr ~~ hyp_8yr
             cd_8yr ~~ cd_8yr
             odd_8yr ~~ odd_8yr
  
           # means of outcomes (freely estimated)
  
             dep_8yr ~ 1
             anx_8yr ~ 1
             inat_8yr ~ 1
             hyp_8yr ~ 1
             cd_8yr ~ 1
             odd_8yr ~ 1
  
           # covariances of outcomes (freely estimated)
  
             dep_8yr ~~ anx_8yr + inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             anx_8yr ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr
             hyp_8yr ~~ cd_8yr + odd_8yr
             cd_8yr ~~ odd_8yr
             
           # covariances of total problem scores with outcomes (fixed to zero)
  
             tot1 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             tot2 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             tot3 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             
           # time-invariant covariate (effects)

             i + s ~ sex

           # time-invariant covariate (intercept & variance)

             sex ~~ sex
             sex ~ 1
          
           # time-invariant covariate (covariances with outcomes)

             sex ~~ dep_8yr + anx_8yr + inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             
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

           # direct effects: c
 
             dep_8yr ~ c_dep_adhd*adhd_child+c_dep_asd*asd_child+c_dep_scz*scz_child+c_dep_bip*bip_child+c_dep_mdd*mdd_child+c_dep_anx*anx_child+c_dep_alc*alc_child+c_dep_ptsd*ptsd_child+c_dep_ocd*ocd_child+c_dep_an*an_child+c_dep_ts*ts_child
             anx_8yr ~ c_anx_adhd*adhd_child+c_anx_asd*asd_child+c_anx_scz*scz_child+c_anx_bip*bip_child+c_anx_mdd*mdd_child+c_anx_anx*anx_child+c_anx_alc*alc_child+c_anx_ptsd*ptsd_child+c_anx_ocd*ocd_child+c_anx_an*an_child+c_anx_ts*ts_child
             inat_8yr ~ c_inat_adhd*adhd_child+c_inat_asd*asd_child+c_inat_scz*scz_child+c_inat_bip*bip_child+c_inat_mdd*mdd_child+c_inat_anx*anx_child+c_inat_alc*alc_child+c_inat_ptsd*ptsd_child+c_inat_ocd*ocd_child+c_inat_an*an_child+c_inat_ts*ts_child
             hyp_8yr ~ c_hyp_adhd*adhd_child+c_hyp_asd*asd_child+c_hyp_scz*scz_child+c_hyp_bip*bip_child+c_hyp_mdd*mdd_child+c_hyp_anx*anx_child+c_hyp_alc*alc_child+c_hyp_ptsd*ptsd_child+c_hyp_ocd*ocd_child+c_hyp_an*an_child+c_hyp_ts*ts_child
             cd_8yr ~ c_cd_adhd*adhd_child+c_cd_asd*asd_child+c_cd_scz*scz_child+c_cd_bip*bip_child+c_cd_mdd*mdd_child+c_cd_anx*anx_child+c_cd_alc*alc_child+c_cd_ptsd*ptsd_child+c_cd_ocd*ocd_child+c_cd_an*an_child+c_cd_ts*ts_child
             odd_8yr ~ c_odd_adhd*adhd_child+c_odd_asd*asd_child+c_odd_scz*scz_child+c_odd_bip*bip_child+c_odd_mdd*mdd_child+c_odd_anx*anx_child+c_odd_alc*alc_child+c_odd_ptsd*ptsd_child+c_odd_ocd*ocd_child+c_odd_an*an_child+c_odd_ts*ts_child
             
           # mediation effects: a & b paths
           
             i ~ a_i_adhd*adhd_child+a_i_asd*asd_child+a_i_scz*scz_child+a_i_bip*bip_child+a_i_mdd*mdd_child+a_i_anx*anx_child+a_i_alc*alc_child+a_i_ptsd*ptsd_child+a_i_ocd*ocd_child+a_i_an*an_child+a_i_ts*ts_child
             s ~ a_s_adhd*adhd_child+a_s_asd*asd_child+a_s_scz*scz_child+a_s_bip*bip_child+a_s_mdd*mdd_child+a_s_anx*anx_child+a_s_alc*alc_child+a_s_ptsd*ptsd_child+a_s_ocd*ocd_child+a_s_an*an_child+a_s_ts*ts_child
             dep_8yr ~ b_i_dep*i + b_s_dep*s
             anx_8yr ~ b_i_anx*i + b_s_anx*s
             inat_8yr ~ b_i_inat*i + b_s_inat*s
             hyp_8yr ~ b_i_hyp*i + b_s_hyp*s
             cd_8yr ~ b_i_cd*i + b_s_cd*s
             odd_8yr ~ b_i_odd*i + b_s_odd*s
             
           # indirect effects: a*b
           
             ab_i_dep_adhd := a_i_adhd*b_i_dep
             ab_s_dep_adhd := a_s_adhd*b_s_dep
             ab_i_anx_adhd := a_i_adhd*b_i_anx
             ab_s_anx_adhd := a_s_adhd*b_s_anx
             ab_i_inat_adhd := a_i_adhd*b_i_inat
             ab_s_inat_adhd := a_s_adhd*b_s_inat
             ab_i_hyp_adhd := a_i_adhd*b_i_hyp
             ab_s_hyp_adhd := a_s_adhd*b_s_hyp
             ab_i_cd_adhd := a_i_adhd*b_i_cd
             ab_s_cd_adhd := a_s_adhd*b_s_cd
             ab_i_odd_adhd := a_i_adhd*b_i_odd
             ab_s_odd_adhd := a_s_adhd*b_s_odd
             
             ab_i_dep_asd := a_i_asd*b_i_dep
             ab_s_dep_asd := a_s_asd*b_s_dep
             ab_i_anx_asd := a_i_asd*b_i_anx
             ab_s_anx_asd := a_s_asd*b_s_anx
             ab_i_inat_asd := a_i_asd*b_i_inat
             ab_s_inat_asd := a_s_asd*b_s_inat
             ab_i_hyp_asd := a_i_asd*b_i_hyp
             ab_s_hyp_asd := a_s_asd*b_s_hyp
             ab_i_cd_asd := a_i_asd*b_i_cd
             ab_s_cd_asd := a_s_asd*b_s_cd
             ab_i_odd_asd := a_i_asd*b_i_odd
             ab_s_odd_asd := a_s_asd*b_s_odd
             
             ab_i_dep_scz := a_i_scz*b_i_dep
             ab_s_dep_scz := a_s_scz*b_s_dep
             ab_i_anx_scz := a_i_scz*b_i_anx
             ab_s_anx_scz := a_s_scz*b_s_anx
             ab_i_inat_scz := a_i_scz*b_i_inat
             ab_s_inat_scz := a_s_scz*b_s_inat
             ab_i_hyp_scz := a_i_scz*b_i_hyp
             ab_s_hyp_scz := a_s_scz*b_s_hyp
             ab_i_cd_scz := a_i_scz*b_i_cd
             ab_s_cd_scz := a_s_scz*b_s_cd
             ab_i_odd_scz := a_i_scz*b_i_odd
             ab_s_odd_scz := a_s_scz*b_s_odd
             
             ab_i_dep_bip := a_i_bip*b_i_dep
             ab_s_dep_bip := a_s_bip*b_s_dep
             ab_i_anx_bip := a_i_bip*b_i_anx
             ab_s_anx_bip := a_s_bip*b_s_anx
             ab_i_inat_bip := a_i_bip*b_i_inat
             ab_s_inat_bip := a_s_bip*b_s_inat
             ab_i_hyp_bip := a_i_bip*b_i_hyp
             ab_s_hyp_bip := a_s_bip*b_s_hyp
             ab_i_cd_bip := a_i_bip*b_i_cd
             ab_s_cd_bip := a_s_bip*b_s_cd
             ab_i_odd_bip := a_i_bip*b_i_odd
             ab_s_odd_bip := a_s_bip*b_s_odd
             
             ab_i_dep_mdd := a_i_mdd*b_i_dep
             ab_s_dep_mdd := a_s_mdd*b_s_dep
             ab_i_anx_mdd := a_i_mdd*b_i_anx
             ab_s_anx_mdd := a_s_mdd*b_s_anx
             ab_i_inat_mdd := a_i_mdd*b_i_inat
             ab_s_inat_mdd := a_s_mdd*b_s_inat
             ab_i_hyp_mdd := a_i_mdd*b_i_hyp
             ab_s_hyp_mdd := a_s_mdd*b_s_hyp
             ab_i_cd_mdd := a_i_mdd*b_i_cd
             ab_s_cd_mdd := a_s_mdd*b_s_cd
             ab_i_odd_mdd := a_i_mdd*b_i_odd
             ab_s_odd_mdd := a_s_mdd*b_s_odd
             
             ab_i_dep_anx := a_i_anx*b_i_dep
             ab_s_dep_anx := a_s_anx*b_s_dep
             ab_i_anx_anx := a_i_anx*b_i_anx
             ab_s_anx_anx := a_s_anx*b_s_anx
             ab_i_inat_anx := a_i_anx*b_i_inat
             ab_s_inat_anx := a_s_anx*b_s_inat
             ab_i_hyp_anx := a_i_anx*b_i_hyp
             ab_s_hyp_anx := a_s_anx*b_s_hyp
             ab_i_cd_anx := a_i_anx*b_i_cd
             ab_s_cd_anx := a_s_anx*b_s_cd
             ab_i_odd_anx := a_i_anx*b_i_odd
             ab_s_odd_anx := a_s_anx*b_s_odd
             
             ab_i_dep_alc := a_i_alc*b_i_dep
             ab_s_dep_alc := a_s_alc*b_s_dep
             ab_i_anx_alc := a_i_alc*b_i_anx
             ab_s_anx_alc := a_s_alc*b_s_anx
             ab_i_inat_alc := a_i_alc*b_i_inat
             ab_s_inat_alc := a_s_alc*b_s_inat
             ab_i_hyp_alc := a_i_alc*b_i_hyp
             ab_s_hyp_alc := a_s_alc*b_s_hyp
             ab_i_cd_alc := a_i_alc*b_i_cd
             ab_s_cd_alc := a_s_alc*b_s_cd
             ab_i_odd_alc := a_i_alc*b_i_odd
             ab_s_odd_alc := a_s_alc*b_s_odd
             
             ab_i_dep_ptsd := a_i_ptsd*b_i_dep
             ab_s_dep_ptsd := a_s_ptsd*b_s_dep
             ab_i_anx_ptsd := a_i_ptsd*b_i_anx
             ab_s_anx_ptsd := a_s_ptsd*b_s_anx
             ab_i_inat_ptsd := a_i_ptsd*b_i_inat
             ab_s_inat_ptsd := a_s_ptsd*b_s_inat
             ab_i_hyp_ptsd := a_i_ptsd*b_i_hyp
             ab_s_hyp_ptsd := a_s_ptsd*b_s_hyp
             ab_i_cd_ptsd := a_i_ptsd*b_i_cd
             ab_s_cd_ptsd := a_s_ptsd*b_s_cd
             ab_i_odd_ptsd := a_i_ptsd*b_i_odd
             ab_s_odd_ptsd := a_s_ptsd*b_s_odd
             
             ab_i_dep_ocd := a_i_ocd*b_i_dep
             ab_s_dep_ocd := a_s_ocd*b_s_dep
             ab_i_anx_ocd := a_i_ocd*b_i_anx
             ab_s_anx_ocd := a_s_ocd*b_s_anx
             ab_i_inat_ocd := a_i_ocd*b_i_inat
             ab_s_inat_ocd := a_s_ocd*b_s_inat
             ab_i_hyp_ocd := a_i_ocd*b_i_hyp
             ab_s_hyp_ocd := a_s_ocd*b_s_hyp
             ab_i_cd_ocd := a_i_ocd*b_i_cd
             ab_s_cd_ocd := a_s_ocd*b_s_cd
             ab_i_odd_ocd := a_i_ocd*b_i_odd
             ab_s_odd_ocd := a_s_ocd*b_s_odd
             
             ab_i_dep_an := a_i_an*b_i_dep
             ab_s_dep_an := a_s_an*b_s_dep
             ab_i_anx_an := a_i_an*b_i_anx
             ab_s_anx_an := a_s_an*b_s_anx
             ab_i_inat_an := a_i_an*b_i_inat
             ab_s_inat_an := a_s_an*b_s_inat
             ab_i_hyp_an := a_i_an*b_i_hyp
             ab_s_hyp_an := a_s_an*b_s_hyp
             ab_i_cd_an := a_i_an*b_i_cd
             ab_s_cd_an := a_s_an*b_s_cd
             ab_i_odd_an := a_i_an*b_i_odd
             ab_s_odd_an := a_s_an*b_s_odd
             
             ab_i_dep_ts := a_i_ts*b_i_dep
             ab_s_dep_ts := a_s_ts*b_s_dep
             ab_i_anx_ts := a_i_ts*b_i_anx
             ab_s_anx_ts := a_s_ts*b_s_anx
             ab_i_inat_ts := a_i_ts*b_i_inat
             ab_s_inat_ts := a_s_ts*b_s_inat
             ab_i_hyp_ts := a_i_ts*b_i_hyp
             ab_s_hyp_ts := a_s_ts*b_s_hyp
             ab_i_cd_ts := a_i_ts*b_i_cd
             ab_s_cd_ts := a_s_ts*b_s_cd
             ab_i_odd_ts := a_i_ts*b_i_odd
             ab_s_odd_ts := a_s_ts*b_s_odd
             
           # total effects: total := c + (a*b)
             
             total_i_dep_adhd := c_dep_adhd + (a_i_adhd*b_i_dep)
             total_s_dep_adhd := c_dep_adhd + (a_s_adhd*b_s_dep)
             total_i_anx_adhd := c_anx_adhd + (a_i_adhd*b_i_anx)
             total_s_anx_adhd := c_anx_adhd + (a_s_adhd*b_s_anx)
             total_i_inat_adhd := c_inat_adhd + (a_i_adhd*b_i_inat)
             total_s_inat_adhd := c_inat_adhd + (a_s_adhd*b_s_inat)
             total_i_hyp_adhd := c_hyp_adhd + (a_i_adhd*b_i_hyp)
             total_s_hyp_adhd := c_hyp_adhd + (a_s_adhd*b_s_hyp)
             total_i_cd_adhd := c_cd_adhd + (a_i_adhd*b_i_cd)
             total_s_cd_adhd := c_cd_adhd + (a_s_adhd*b_s_cd)
             total_i_odd_adhd := c_odd_adhd + (a_i_adhd*b_i_odd)
             total_s_odd_adhd := c_odd_adhd + (a_s_adhd*b_s_odd)
             
             total_i_dep_asd := c_dep_asd + (a_i_asd*b_i_dep)
             total_s_dep_asd := c_dep_asd + (a_s_asd*b_s_dep)
             total_i_anx_asd := c_anx_asd + (a_i_asd*b_i_anx)
             total_s_anx_asd := c_anx_asd + (a_s_asd*b_s_anx)
             total_i_inat_asd := c_inat_asd + (a_i_asd*b_i_inat)
             total_s_inat_asd := c_inat_asd + (a_s_asd*b_s_inat)
             total_i_hyp_asd := c_hyp_asd + (a_i_asd*b_i_hyp)
             total_s_hyp_asd := c_hyp_asd + (a_s_asd*b_s_hyp)
             total_i_cd_asd := c_cd_asd + (a_i_asd*b_i_cd)
             total_s_cd_asd := c_cd_asd + (a_s_asd*b_s_cd)
             total_i_odd_asd := c_odd_asd + (a_i_asd*b_i_odd)
             total_s_odd_asd := c_odd_asd + (a_s_asd*b_s_odd)
             
             total_i_dep_scz := c_dep_scz + (a_i_scz*b_i_dep)
             total_s_dep_scz := c_dep_scz + (a_s_scz*b_s_dep)
             total_i_anx_scz := c_anx_scz + (a_i_scz*b_i_anx)
             total_s_anx_scz := c_anx_scz + (a_s_scz*b_s_anx)
             total_i_inat_scz := c_inat_scz + (a_i_scz*b_i_inat)
             total_s_inat_scz := c_inat_scz + (a_s_scz*b_s_inat)
             total_i_hyp_scz := c_hyp_scz + (a_i_scz*b_i_hyp)
             total_s_hyp_scz := c_hyp_scz + (a_s_scz*b_s_hyp)
             total_i_cd_scz := c_cd_scz + (a_i_scz*b_i_cd)
             total_s_cd_scz := c_cd_scz + (a_s_scz*b_s_cd)
             total_i_odd_scz := c_odd_scz + (a_i_scz*b_i_odd)
             total_s_odd_scz := c_odd_scz + (a_s_scz*b_s_odd)
             
             total_i_dep_bip := c_dep_bip + (a_i_bip*b_i_dep)
             total_s_dep_bip := c_dep_bip + (a_s_bip*b_s_dep)
             total_i_anx_bip := c_anx_bip + (a_i_bip*b_i_anx)
             total_s_anx_bip := c_anx_bip + (a_s_bip*b_s_anx)
             total_i_inat_bip := c_inat_bip + (a_i_bip*b_i_inat)
             total_s_inat_bip := c_inat_bip + (a_s_bip*b_s_inat)
             total_i_hyp_bip := c_hyp_bip + (a_i_bip*b_i_hyp)
             total_s_hyp_bip := c_hyp_bip + (a_s_bip*b_s_hyp)
             total_i_cd_bip := c_cd_bip + (a_i_bip*b_i_cd)
             total_s_cd_bip := c_cd_bip + (a_s_bip*b_s_cd)
             total_i_odd_bip := c_odd_bip + (a_i_bip*b_i_odd)
             total_s_odd_bip := c_odd_bip + (a_s_bip*b_s_odd)
             
             total_i_dep_mdd := c_dep_mdd + (a_i_mdd*b_i_dep)
             total_s_dep_mdd := c_dep_mdd + (a_s_mdd*b_s_dep)
             total_i_anx_mdd := c_anx_mdd + (a_i_mdd*b_i_anx)
             total_s_anx_mdd := c_anx_mdd + (a_s_mdd*b_s_anx)
             total_i_inat_mdd := c_inat_mdd + (a_i_mdd*b_i_inat)
             total_s_inat_mdd := c_inat_mdd + (a_s_mdd*b_s_inat)
             total_i_hyp_mdd := c_hyp_mdd + (a_i_mdd*b_i_hyp)
             total_s_hyp_mdd := c_hyp_mdd + (a_s_mdd*b_s_hyp)
             total_i_cd_mdd := c_cd_mdd + (a_i_mdd*b_i_cd)
             total_s_cd_mdd := c_cd_mdd + (a_s_mdd*b_s_cd)
             total_i_odd_mdd := c_odd_mdd + (a_i_mdd*b_i_odd)
             total_s_odd_mdd := c_odd_mdd + (a_s_mdd*b_s_odd)
             
             total_i_dep_anx := c_dep_anx + (a_i_anx*b_i_dep)
             total_s_dep_anx := c_dep_anx + (a_s_anx*b_s_dep)
             total_i_anx_anx := c_anx_anx + (a_i_anx*b_i_anx)
             total_s_anx_anx := c_anx_anx + (a_s_anx*b_s_anx)
             total_i_inat_anx := c_inat_anx + (a_i_anx*b_i_inat)
             total_s_inat_anx := c_inat_anx + (a_s_anx*b_s_inat)
             total_i_hyp_anx := c_hyp_anx + (a_i_anx*b_i_hyp)
             total_s_hyp_anx := c_hyp_anx + (a_s_anx*b_s_hyp)
             total_i_cd_anx := c_cd_anx + (a_i_anx*b_i_cd)
             total_s_cd_anx := c_cd_anx + (a_s_anx*b_s_cd)
             total_i_odd_anx := c_odd_anx + (a_i_anx*b_i_odd)
             total_s_odd_anx := c_odd_anx + (a_s_anx*b_s_odd)
             
             total_i_dep_alc := c_dep_alc + (a_i_alc*b_i_dep)
             total_s_dep_alc := c_dep_alc + (a_s_alc*b_s_dep)
             total_i_anx_alc := c_anx_alc + (a_i_alc*b_i_anx)
             total_s_anx_alc := c_anx_alc + (a_s_alc*b_s_anx)
             total_i_inat_alc := c_inat_alc + (a_i_alc*b_i_inat)
             total_s_inat_alc := c_inat_alc + (a_s_alc*b_s_inat)
             total_i_hyp_alc := c_hyp_alc + (a_i_alc*b_i_hyp)
             total_s_hyp_alc := c_hyp_alc + (a_s_alc*b_s_hyp)
             total_i_cd_alc := c_cd_alc + (a_i_alc*b_i_cd)
             total_s_cd_alc := c_cd_alc + (a_s_alc*b_s_cd)
             total_i_odd_alc := c_odd_alc + (a_i_alc*b_i_odd)
             total_s_odd_alc := c_odd_alc + (a_s_alc*b_s_odd)
             
             total_i_dep_ptsd := c_dep_ptsd + (a_i_ptsd*b_i_dep)
             total_s_dep_ptsd := c_dep_ptsd + (a_s_ptsd*b_s_dep)
             total_i_anx_ptsd := c_anx_ptsd + (a_i_ptsd*b_i_anx)
             total_s_anx_ptsd := c_anx_ptsd + (a_s_ptsd*b_s_anx)
             total_i_inat_ptsd := c_inat_ptsd + (a_i_ptsd*b_i_inat)
             total_s_inat_ptsd := c_inat_ptsd + (a_s_ptsd*b_s_inat)
             total_i_hyp_ptsd := c_hyp_ptsd + (a_i_ptsd*b_i_hyp)
             total_s_hyp_ptsd := c_hyp_ptsd + (a_s_ptsd*b_s_hyp)
             total_i_cd_ptsd := c_cd_ptsd + (a_i_ptsd*b_i_cd)
             total_s_cd_ptsd := c_cd_ptsd + (a_s_ptsd*b_s_cd)
             total_i_odd_ptsd := c_odd_ptsd + (a_i_ptsd*b_i_odd)
             total_s_odd_ptsd := c_odd_ptsd + (a_s_ptsd*b_s_odd)
             
             total_i_dep_ocd := c_dep_ocd + (a_i_ocd*b_i_dep)
             total_s_dep_ocd := c_dep_ocd + (a_s_ocd*b_s_dep)
             total_i_anx_ocd := c_anx_ocd + (a_i_ocd*b_i_anx)
             total_s_anx_ocd := c_anx_ocd + (a_s_ocd*b_s_anx)
             total_i_inat_ocd := c_inat_ocd + (a_i_ocd*b_i_inat)
             total_s_inat_ocd := c_inat_ocd + (a_s_ocd*b_s_inat)
             total_i_hyp_ocd := c_hyp_ocd + (a_i_ocd*b_i_hyp)
             total_s_hyp_ocd := c_hyp_ocd + (a_s_ocd*b_s_hyp)
             total_i_cd_ocd := c_cd_ocd + (a_i_ocd*b_i_cd)
             total_s_cd_ocd := c_cd_ocd + (a_s_ocd*b_s_cd)
             total_i_odd_ocd := c_odd_ocd + (a_i_ocd*b_i_odd)
             total_s_odd_ocd := c_odd_ocd + (a_s_ocd*b_s_odd)
             
             total_i_dep_an := c_dep_an + (a_i_an*b_i_dep)
             total_s_dep_an := c_dep_an + (a_s_an*b_s_dep)
             total_i_anx_an := c_anx_an + (a_i_an*b_i_anx)
             total_s_anx_an := c_anx_an + (a_s_an*b_s_anx)
             total_i_inat_an := c_inat_an + (a_i_an*b_i_inat)
             total_s_inat_an := c_inat_an + (a_s_an*b_s_inat)
             total_i_hyp_an := c_hyp_an + (a_i_an*b_i_hyp)
             total_s_hyp_an := c_hyp_an + (a_s_an*b_s_hyp)
             total_i_cd_an := c_cd_an + (a_i_an*b_i_cd)
             total_s_cd_an := c_cd_an + (a_s_an*b_s_cd)
             total_i_odd_an := c_odd_an + (a_i_an*b_i_odd)
             total_s_odd_an := c_odd_an + (a_s_an*b_s_odd)
             
             total_i_dep_ts := c_dep_ts + (a_i_ts*b_i_dep)
             total_s_dep_ts := c_dep_ts + (a_s_ts*b_s_dep)
             total_i_anx_ts := c_anx_ts + (a_i_ts*b_i_anx)
             total_s_anx_ts := c_anx_ts + (a_s_ts*b_s_anx)
             total_i_inat_ts := c_inat_ts + (a_i_ts*b_i_inat)
             total_s_inat_ts := c_inat_ts + (a_s_ts*b_s_inat)
             total_i_hyp_ts := c_hyp_ts + (a_i_ts*b_i_hyp)
             total_s_hyp_ts := c_hyp_ts + (a_s_ts*b_s_hyp)
             total_i_cd_ts := c_cd_ts + (a_i_ts*b_i_cd)
             total_s_cd_ts := c_cd_ts + (a_s_ts*b_s_cd)
             total_i_odd_ts := c_odd_ts + (a_i_ts*b_i_odd)
             total_s_odd_ts := c_odd_ts + (a_s_ts*b_s_odd)
             
             '
 
# both differentiation and total mediating relationship between PGS and symptoms
 
model_both_sympt <-' 
           # growth parameters (latent variables)
  
             i1 =~ 1*diff1 + 1*diff2 + 1*diff3
             s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
             i2 =~ 1*tot1 + 1*tot2 + 1*tot3
             s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
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
             
           # obs variable variances

             diff1 ~~ diff1
             diff2 ~~ diff2
             diff3 ~~ diff3
             tot1 ~~ tot1
             tot2 ~~ tot2
             tot3 ~~ tot3
             dep_8yr ~~ dep_8yr
             anx_8yr ~~ anx_8yr
             inat_8yr ~~ inat_8yr
             hyp_8yr ~~ hyp_8yr
             cd_8yr ~~ cd_8yr
             odd_8yr ~~ odd_8yr
  
           # means of outcomes (freely estimated)
  
             dep_8yr ~ 1
             anx_8yr ~ 1
             inat_8yr ~ 1
             hyp_8yr ~ 1
             cd_8yr ~ 1
             odd_8yr ~ 1
  
           # covariances of outcomes (freely estimated)
  
             dep_8yr ~~ anx_8yr + inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             anx_8yr ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr
             hyp_8yr ~~ cd_8yr + odd_8yr
             cd_8yr ~~ odd_8yr
             
           # covariances of diff + tot scores with outcomes (fixed to zero)
  
             diff1 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             diff2 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             diff3 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             tot1 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             tot2 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             tot3 ~~ 0*dep_8yr + 0*anx_8yr + 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr
             
           # time-invariant covariate (effects)

             i1 + s1 + i2 + s2 ~ sex

           # time-invariant covariate (intercept & variance)

             sex ~~ sex
             sex ~ 1
          
           # time-invariant covariate (covariances with outcomes)

             sex ~~ dep_8yr + anx_8yr + inat_8yr + hyp_8yr + cd_8yr + odd_8yr
             
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

           # direct effects: c
 
             dep_8yr ~ c_dep_adhd*adhd_child+c_dep_asd*asd_child+c_dep_scz*scz_child+c_dep_bip*bip_child+c_dep_mdd*mdd_child+c_dep_anx*anx_child+c_dep_alc*alc_child+c_dep_ptsd*ptsd_child+c_dep_ocd*ocd_child+c_dep_an*an_child+c_dep_ts*ts_child
             anx_8yr ~ c_anx_adhd*adhd_child+c_anx_asd*asd_child+c_anx_scz*scz_child+c_anx_bip*bip_child+c_anx_mdd*mdd_child+c_anx_anx*anx_child+c_anx_alc*alc_child+c_anx_ptsd*ptsd_child+c_anx_ocd*ocd_child+c_anx_an*an_child+c_anx_ts*ts_child
             inat_8yr ~ c_inat_adhd*adhd_child+c_inat_asd*asd_child+c_inat_scz*scz_child+c_inat_bip*bip_child+c_inat_mdd*mdd_child+c_inat_anx*anx_child+c_inat_alc*alc_child+c_inat_ptsd*ptsd_child+c_inat_ocd*ocd_child+c_inat_an*an_child+c_inat_ts*ts_child
             hyp_8yr ~ c_hyp_adhd*adhd_child+c_hyp_asd*asd_child+c_hyp_scz*scz_child+c_hyp_bip*bip_child+c_hyp_mdd*mdd_child+c_hyp_anx*anx_child+c_hyp_alc*alc_child+c_hyp_ptsd*ptsd_child+c_hyp_ocd*ocd_child+c_hyp_an*an_child+c_hyp_ts*ts_child
             cd_8yr ~ c_cd_adhd*adhd_child+c_cd_asd*asd_child+c_cd_scz*scz_child+c_cd_bip*bip_child+c_cd_mdd*mdd_child+c_cd_anx*anx_child+c_cd_alc*alc_child+c_cd_ptsd*ptsd_child+c_cd_ocd*ocd_child+c_cd_an*an_child+c_cd_ts*ts_child
             odd_8yr ~ c_odd_adhd*adhd_child+c_odd_asd*asd_child+c_odd_scz*scz_child+c_odd_bip*bip_child+c_odd_mdd*mdd_child+c_odd_anx*anx_child+c_odd_alc*alc_child+c_odd_ptsd*ptsd_child+c_odd_ocd*ocd_child+c_odd_an*an_child+c_odd_ts*ts_child
             
           # mediation effects: a & b paths
           
             i1 ~ a_i1_adhd*adhd_child+a_i1_asd*asd_child+a_i1_scz*scz_child+a_i1_bip*bip_child+a_i1_mdd*mdd_child+a_i1_anx*anx_child+a_i1_alc*alc_child+a_i1_ptsd*ptsd_child+a_i1_ocd*ocd_child+a_i1_an*an_child+a_i1_ts*ts_child
             s1 ~ a_s1_adhd*adhd_child+a_s1_asd*asd_child+a_s1_scz*scz_child+a_s1_bip*bip_child+a_s1_mdd*mdd_child+a_s1_anx*anx_child+a_s1_alc*alc_child+a_s1_ptsd*ptsd_child+a_s1_ocd*ocd_child+a_s1_an*an_child+a_s1_ts*ts_child
             dep_8yr ~ b_i1_dep*i1 + b_s1_dep*s1
             anx_8yr ~ b_i1_anx*i1 + b_s1_anx*s1
             inat_8yr ~ b_i1_inat*i1 + b_s1_inat*s1
             hyp_8yr ~ b_i1_hyp*i1 + b_s1_hyp*s1
             cd_8yr ~ b_i1_cd*i1 + b_s1_cd*s1
             odd_8yr ~ b_i1_odd*i1 + b_s1_odd*s1
             
             i2 ~ a_i2_adhd*adhd_child+a_i2_asd*asd_child+a_i2_scz*scz_child+a_i2_bip*bip_child+a_i2_mdd*mdd_child+a_i2_anx*anx_child+a_i2_alc*alc_child+a_i2_ptsd*ptsd_child+a_i2_ocd*ocd_child+a_i2_an*an_child+a_i2_ts*ts_child
             s2 ~ a_s2_adhd*adhd_child+a_s2_asd*asd_child+a_s2_scz*scz_child+a_s2_bip*bip_child+a_s2_mdd*mdd_child+a_s2_anx*anx_child+a_s2_alc*alc_child+a_s2_ptsd*ptsd_child+a_s2_ocd*ocd_child+a_s2_an*an_child+a_s2_ts*ts_child
             dep_8yr ~ b_i2_dep*i2 + b_s2_dep*s2
             anx_8yr ~ b_i2_anx*i2 + b_s2_anx*s2
             inat_8yr ~ b_i2_inat*i2 + b_s2_inat*s2
             hyp_8yr ~ b_i2_hyp*i2 + b_s2_hyp*s2
             cd_8yr ~ b_i2_cd*i2 + b_s2_cd*s2
             odd_8yr ~ b_i2_odd*i2 + b_s2_odd*s2
             
           # indirect effects: a*b
           
             ab_i1_dep_adhd := a_i1_adhd*b_i1_dep
             ab_s1_dep_adhd := a_s1_adhd*b_s1_dep
             ab_i1_anx_adhd := a_i1_adhd*b_i1_anx
             ab_s1_anx_adhd := a_s1_adhd*b_s1_anx
             ab_i1_inat_adhd := a_i1_adhd*b_i1_inat
             ab_s1_inat_adhd := a_s1_adhd*b_s1_inat
             ab_i1_hyp_adhd := a_i1_adhd*b_i1_hyp
             ab_s1_hyp_adhd := a_s1_adhd*b_s1_hyp
             ab_i1_cd_adhd := a_i1_adhd*b_i1_cd
             ab_s1_cd_adhd := a_s1_adhd*b_s1_cd
             ab_i1_odd_adhd := a_i1_adhd*b_i1_odd
             ab_s1_odd_adhd := a_s1_adhd*b_s1_odd
             
             ab_i1_dep_asd := a_i1_asd*b_i1_dep
             ab_s1_dep_asd := a_s1_asd*b_s1_dep
             ab_i1_anx_asd := a_i1_asd*b_i1_anx
             ab_s1_anx_asd := a_s1_asd*b_s1_anx
             ab_i1_inat_asd := a_i1_asd*b_i1_inat
             ab_s1_inat_asd := a_s1_asd*b_s1_inat
             ab_i1_hyp_asd := a_i1_asd*b_i1_hyp
             ab_s1_hyp_asd := a_s1_asd*b_s1_hyp
             ab_i1_cd_asd := a_i1_asd*b_i1_cd
             ab_s1_cd_asd := a_s1_asd*b_s1_cd
             ab_i1_odd_asd := a_i1_asd*b_i1_odd
             ab_s1_odd_asd := a_s1_asd*b_s1_odd
             
             ab_i1_dep_scz := a_i1_scz*b_i1_dep
             ab_s1_dep_scz := a_s1_scz*b_s1_dep
             ab_i1_anx_scz := a_i1_scz*b_i1_anx
             ab_s1_anx_scz := a_s1_scz*b_s1_anx
             ab_i1_inat_scz := a_i1_scz*b_i1_inat
             ab_s1_inat_scz := a_s1_scz*b_s1_inat
             ab_i1_hyp_scz := a_i1_scz*b_i1_hyp
             ab_s1_hyp_scz := a_s1_scz*b_s1_hyp
             ab_i1_cd_scz := a_i1_scz*b_i1_cd
             ab_s1_cd_scz := a_s1_scz*b_s1_cd
             ab_i1_odd_scz := a_i1_scz*b_i1_odd
             ab_s1_odd_scz := a_s1_scz*b_s1_odd
             
             ab_i1_dep_bip := a_i1_bip*b_i1_dep
             ab_s1_dep_bip := a_s1_bip*b_s1_dep
             ab_i1_anx_bip := a_i1_bip*b_i1_anx
             ab_s1_anx_bip := a_s1_bip*b_s1_anx
             ab_i1_inat_bip := a_i1_bip*b_i1_inat
             ab_s1_inat_bip := a_s1_bip*b_s1_inat
             ab_i1_hyp_bip := a_i1_bip*b_i1_hyp
             ab_s1_hyp_bip := a_s1_bip*b_s1_hyp
             ab_i1_cd_bip := a_i1_bip*b_i1_cd
             ab_s1_cd_bip := a_s1_bip*b_s1_cd
             ab_i1_odd_bip := a_i1_bip*b_i1_odd
             ab_s1_odd_bip := a_s1_bip*b_s1_odd
             
             ab_i1_dep_mdd := a_i1_mdd*b_i1_dep
             ab_s1_dep_mdd := a_s1_mdd*b_s1_dep
             ab_i1_anx_mdd := a_i1_mdd*b_i1_anx
             ab_s1_anx_mdd := a_s1_mdd*b_s1_anx
             ab_i1_inat_mdd := a_i1_mdd*b_i1_inat
             ab_s1_inat_mdd := a_s1_mdd*b_s1_inat
             ab_i1_hyp_mdd := a_i1_mdd*b_i1_hyp
             ab_s1_hyp_mdd := a_s1_mdd*b_s1_hyp
             ab_i1_cd_mdd := a_i1_mdd*b_i1_cd
             ab_s1_cd_mdd := a_s1_mdd*b_s1_cd
             ab_i1_odd_mdd := a_i1_mdd*b_i1_odd
             ab_s1_odd_mdd := a_s1_mdd*b_s1_odd
             
             ab_i1_dep_anx := a_i1_anx*b_i1_dep
             ab_s1_dep_anx := a_s1_anx*b_s1_dep
             ab_i1_anx_anx := a_i1_anx*b_i1_anx
             ab_s1_anx_anx := a_s1_anx*b_s1_anx
             ab_i1_inat_anx := a_i1_anx*b_i1_inat
             ab_s1_inat_anx := a_s1_anx*b_s1_inat
             ab_i1_hyp_anx := a_i1_anx*b_i1_hyp
             ab_s1_hyp_anx := a_s1_anx*b_s1_hyp
             ab_i1_cd_anx := a_i1_anx*b_i1_cd
             ab_s1_cd_anx := a_s1_anx*b_s1_cd
             ab_i1_odd_anx := a_i1_anx*b_i1_odd
             ab_s1_odd_anx := a_s1_anx*b_s1_odd
             
             ab_i1_dep_alc := a_i1_alc*b_i1_dep
             ab_s1_dep_alc := a_s1_alc*b_s1_dep
             ab_i1_anx_alc := a_i1_alc*b_i1_anx
             ab_s1_anx_alc := a_s1_alc*b_s1_anx
             ab_i1_inat_alc := a_i1_alc*b_i1_inat
             ab_s1_inat_alc := a_s1_alc*b_s1_inat
             ab_i1_hyp_alc := a_i1_alc*b_i1_hyp
             ab_s1_hyp_alc := a_s1_alc*b_s1_hyp
             ab_i1_cd_alc := a_i1_alc*b_i1_cd
             ab_s1_cd_alc := a_s1_alc*b_s1_cd
             ab_i1_odd_alc := a_i1_alc*b_i1_odd
             ab_s1_odd_alc := a_s1_alc*b_s1_odd
             
             ab_i1_dep_ptsd := a_i1_ptsd*b_i1_dep
             ab_s1_dep_ptsd := a_s1_ptsd*b_s1_dep
             ab_i1_anx_ptsd := a_i1_ptsd*b_i1_anx
             ab_s1_anx_ptsd := a_s1_ptsd*b_s1_anx
             ab_i1_inat_ptsd := a_i1_ptsd*b_i1_inat
             ab_s1_inat_ptsd := a_s1_ptsd*b_s1_inat
             ab_i1_hyp_ptsd := a_i1_ptsd*b_i1_hyp
             ab_s1_hyp_ptsd := a_s1_ptsd*b_s1_hyp
             ab_i1_cd_ptsd := a_i1_ptsd*b_i1_cd
             ab_s1_cd_ptsd := a_s1_ptsd*b_s1_cd
             ab_i1_odd_ptsd := a_i1_ptsd*b_i1_odd
             ab_s1_odd_ptsd := a_s1_ptsd*b_s1_odd
             
             ab_i1_dep_ocd := a_i1_ocd*b_i1_dep
             ab_s1_dep_ocd := a_s1_ocd*b_s1_dep
             ab_i1_anx_ocd := a_i1_ocd*b_i1_anx
             ab_s1_anx_ocd := a_s1_ocd*b_s1_anx
             ab_i1_inat_ocd := a_i1_ocd*b_i1_inat
             ab_s1_inat_ocd := a_s1_ocd*b_s1_inat
             ab_i1_hyp_ocd := a_i1_ocd*b_i1_hyp
             ab_s1_hyp_ocd := a_s1_ocd*b_s1_hyp
             ab_i1_cd_ocd := a_i1_ocd*b_i1_cd
             ab_s1_cd_ocd := a_s1_ocd*b_s1_cd
             ab_i1_odd_ocd := a_i1_ocd*b_i1_odd
             ab_s1_odd_ocd := a_s1_ocd*b_s1_odd
             
             ab_i1_dep_an := a_i1_an*b_i1_dep
             ab_s1_dep_an := a_s1_an*b_s1_dep
             ab_i1_anx_an := a_i1_an*b_i1_anx
             ab_s1_anx_an := a_s1_an*b_s1_anx
             ab_i1_inat_an := a_i1_an*b_i1_inat
             ab_s1_inat_an := a_s1_an*b_s1_inat
             ab_i1_hyp_an := a_i1_an*b_i1_hyp
             ab_s1_hyp_an := a_s1_an*b_s1_hyp
             ab_i1_cd_an := a_i1_an*b_i1_cd
             ab_s1_cd_an := a_s1_an*b_s1_cd
             ab_i1_odd_an := a_i1_an*b_i1_odd
             ab_s1_odd_an := a_s1_an*b_s1_odd
             
             ab_i1_dep_ts := a_i1_ts*b_i1_dep
             ab_s1_dep_ts := a_s1_ts*b_s1_dep
             ab_i1_anx_ts := a_i1_ts*b_i1_anx
             ab_s1_anx_ts := a_s1_ts*b_s1_anx
             ab_i1_inat_ts := a_i1_ts*b_i1_inat
             ab_s1_inat_ts := a_s1_ts*b_s1_inat
             ab_i1_hyp_ts := a_i1_ts*b_i1_hyp
             ab_s1_hyp_ts := a_s1_ts*b_s1_hyp
             ab_i1_cd_ts := a_i1_ts*b_i1_cd
             ab_s1_cd_ts := a_s1_ts*b_s1_cd
             ab_i1_odd_ts := a_i1_ts*b_i1_odd
             ab_s1_odd_ts := a_s1_ts*b_s1_odd
             
             ab_i2_dep_adhd := a_i2_adhd*b_i2_dep
             ab_s2_dep_adhd := a_s2_adhd*b_s2_dep
             ab_i2_anx_adhd := a_i2_adhd*b_i2_anx
             ab_s2_anx_adhd := a_s2_adhd*b_s2_anx
             ab_i2_inat_adhd := a_i2_adhd*b_i2_inat
             ab_s2_inat_adhd := a_s2_adhd*b_s2_inat
             ab_i2_hyp_adhd := a_i2_adhd*b_i2_hyp
             ab_s2_hyp_adhd := a_s2_adhd*b_s2_hyp
             ab_i2_cd_adhd := a_i2_adhd*b_i2_cd
             ab_s2_cd_adhd := a_s2_adhd*b_s2_cd
             ab_i2_odd_adhd := a_i2_adhd*b_i2_odd
             ab_s2_odd_adhd := a_s2_adhd*b_s2_odd
             
             ab_i2_dep_asd := a_i2_asd*b_i2_dep
             ab_s2_dep_asd := a_s2_asd*b_s2_dep
             ab_i2_anx_asd := a_i2_asd*b_i2_anx
             ab_s2_anx_asd := a_s2_asd*b_s2_anx
             ab_i2_inat_asd := a_i2_asd*b_i2_inat
             ab_s2_inat_asd := a_s2_asd*b_s2_inat
             ab_i2_hyp_asd := a_i2_asd*b_i2_hyp
             ab_s2_hyp_asd := a_s2_asd*b_s2_hyp
             ab_i2_cd_asd := a_i2_asd*b_i2_cd
             ab_s2_cd_asd := a_s2_asd*b_s2_cd
             ab_i2_odd_asd := a_i2_asd*b_i2_odd
             ab_s2_odd_asd := a_s2_asd*b_s2_odd
             
             ab_i2_dep_scz := a_i2_scz*b_i2_dep
             ab_s2_dep_scz := a_s2_scz*b_s2_dep
             ab_i2_anx_scz := a_i2_scz*b_i2_anx
             ab_s2_anx_scz := a_s2_scz*b_s2_anx
             ab_i2_inat_scz := a_i2_scz*b_i2_inat
             ab_s2_inat_scz := a_s2_scz*b_s2_inat
             ab_i2_hyp_scz := a_i2_scz*b_i2_hyp
             ab_s2_hyp_scz := a_s2_scz*b_s2_hyp
             ab_i2_cd_scz := a_i2_scz*b_i2_cd
             ab_s2_cd_scz := a_s2_scz*b_s2_cd
             ab_i2_odd_scz := a_i2_scz*b_i2_odd
             ab_s2_odd_scz := a_s2_scz*b_s2_odd
             
             ab_i2_dep_bip := a_i2_bip*b_i2_dep
             ab_s2_dep_bip := a_s2_bip*b_s2_dep
             ab_i2_anx_bip := a_i2_bip*b_i2_anx
             ab_s2_anx_bip := a_s2_bip*b_s2_anx
             ab_i2_inat_bip := a_i2_bip*b_i2_inat
             ab_s2_inat_bip := a_s2_bip*b_s2_inat
             ab_i2_hyp_bip := a_i2_bip*b_i2_hyp
             ab_s2_hyp_bip := a_s2_bip*b_s2_hyp
             ab_i2_cd_bip := a_i2_bip*b_i2_cd
             ab_s2_cd_bip := a_s2_bip*b_s2_cd
             ab_i2_odd_bip := a_i2_bip*b_i2_odd
             ab_s2_odd_bip := a_s2_bip*b_s2_odd
             
             ab_i2_dep_mdd := a_i2_mdd*b_i2_dep
             ab_s2_dep_mdd := a_s2_mdd*b_s2_dep
             ab_i2_anx_mdd := a_i2_mdd*b_i2_anx
             ab_s2_anx_mdd := a_s2_mdd*b_s2_anx
             ab_i2_inat_mdd := a_i2_mdd*b_i2_inat
             ab_s2_inat_mdd := a_s2_mdd*b_s2_inat
             ab_i2_hyp_mdd := a_i2_mdd*b_i2_hyp
             ab_s2_hyp_mdd := a_s2_mdd*b_s2_hyp
             ab_i2_cd_mdd := a_i2_mdd*b_i2_cd
             ab_s2_cd_mdd := a_s2_mdd*b_s2_cd
             ab_i2_odd_mdd := a_i2_mdd*b_i2_odd
             ab_s2_odd_mdd := a_s2_mdd*b_s2_odd
             
             ab_i2_dep_anx := a_i2_anx*b_i2_dep
             ab_s2_dep_anx := a_s2_anx*b_s2_dep
             ab_i2_anx_anx := a_i2_anx*b_i2_anx
             ab_s2_anx_anx := a_s2_anx*b_s2_anx
             ab_i2_inat_anx := a_i2_anx*b_i2_inat
             ab_s2_inat_anx := a_s2_anx*b_s2_inat
             ab_i2_hyp_anx := a_i2_anx*b_i2_hyp
             ab_s2_hyp_anx := a_s2_anx*b_s2_hyp
             ab_i2_cd_anx := a_i2_anx*b_i2_cd
             ab_s2_cd_anx := a_s2_anx*b_s2_cd
             ab_i2_odd_anx := a_i2_anx*b_i2_odd
             ab_s2_odd_anx := a_s2_anx*b_s2_odd
             
             ab_i2_dep_alc := a_i2_alc*b_i2_dep
             ab_s2_dep_alc := a_s2_alc*b_s2_dep
             ab_i2_anx_alc := a_i2_alc*b_i2_anx
             ab_s2_anx_alc := a_s2_alc*b_s2_anx
             ab_i2_inat_alc := a_i2_alc*b_i2_inat
             ab_s2_inat_alc := a_s2_alc*b_s2_inat
             ab_i2_hyp_alc := a_i2_alc*b_i2_hyp
             ab_s2_hyp_alc := a_s2_alc*b_s2_hyp
             ab_i2_cd_alc := a_i2_alc*b_i2_cd
             ab_s2_cd_alc := a_s2_alc*b_s2_cd
             ab_i2_odd_alc := a_i2_alc*b_i2_odd
             ab_s2_odd_alc := a_s2_alc*b_s2_odd
             
             ab_i2_dep_ptsd := a_i2_ptsd*b_i2_dep
             ab_s2_dep_ptsd := a_s2_ptsd*b_s2_dep
             ab_i2_anx_ptsd := a_i2_ptsd*b_i2_anx
             ab_s2_anx_ptsd := a_s2_ptsd*b_s2_anx
             ab_i2_inat_ptsd := a_i2_ptsd*b_i2_inat
             ab_s2_inat_ptsd := a_s2_ptsd*b_s2_inat
             ab_i2_hyp_ptsd := a_i2_ptsd*b_i2_hyp
             ab_s2_hyp_ptsd := a_s2_ptsd*b_s2_hyp
             ab_i2_cd_ptsd := a_i2_ptsd*b_i2_cd
             ab_s2_cd_ptsd := a_s2_ptsd*b_s2_cd
             ab_i2_odd_ptsd := a_i2_ptsd*b_i2_odd
             ab_s2_odd_ptsd := a_s2_ptsd*b_s2_odd
             
             ab_i2_dep_ocd := a_i2_ocd*b_i2_dep
             ab_s2_dep_ocd := a_s2_ocd*b_s2_dep
             ab_i2_anx_ocd := a_i2_ocd*b_i2_anx
             ab_s2_anx_ocd := a_s2_ocd*b_s2_anx
             ab_i2_inat_ocd := a_i2_ocd*b_i2_inat
             ab_s2_inat_ocd := a_s2_ocd*b_s2_inat
             ab_i2_hyp_ocd := a_i2_ocd*b_i2_hyp
             ab_s2_hyp_ocd := a_s2_ocd*b_s2_hyp
             ab_i2_cd_ocd := a_i2_ocd*b_i2_cd
             ab_s2_cd_ocd := a_s2_ocd*b_s2_cd
             ab_i2_odd_ocd := a_i2_ocd*b_i2_odd
             ab_s2_odd_ocd := a_s2_ocd*b_s2_odd
             
             ab_i2_dep_an := a_i2_an*b_i2_dep
             ab_s2_dep_an := a_s2_an*b_s2_dep
             ab_i2_anx_an := a_i2_an*b_i2_anx
             ab_s2_anx_an := a_s2_an*b_s2_anx
             ab_i2_inat_an := a_i2_an*b_i2_inat
             ab_s2_inat_an := a_s2_an*b_s2_inat
             ab_i2_hyp_an := a_i2_an*b_i2_hyp
             ab_s2_hyp_an := a_s2_an*b_s2_hyp
             ab_i2_cd_an := a_i2_an*b_i2_cd
             ab_s2_cd_an := a_s2_an*b_s2_cd
             ab_i2_odd_an := a_i2_an*b_i2_odd
             ab_s2_odd_an := a_s2_an*b_s2_odd
             
             ab_i2_dep_ts := a_i2_ts*b_i2_dep
             ab_s2_dep_ts := a_s2_ts*b_s2_dep
             ab_i2_anx_ts := a_i2_ts*b_i2_anx
             ab_s2_anx_ts := a_s2_ts*b_s2_anx
             ab_i2_inat_ts := a_i2_ts*b_i2_inat
             ab_s2_inat_ts := a_s2_ts*b_s2_inat
             ab_i2_hyp_ts := a_i2_ts*b_i2_hyp
             ab_s2_hyp_ts := a_s2_ts*b_s2_hyp
             ab_i2_cd_ts := a_i2_ts*b_i2_cd
             ab_s2_cd_ts := a_s2_ts*b_s2_cd
             ab_i2_odd_ts := a_i2_ts*b_i2_odd
             ab_s2_odd_ts := a_s2_ts*b_s2_odd
             
           # total effects: total := c + (a*b)
             
             total_i1_dep_adhd := c_dep_adhd + (a_i1_adhd*b_i1_dep)
             total_s1_dep_adhd := c_dep_adhd + (a_s1_adhd*b_s1_dep)
             total_i1_anx_adhd := c_anx_adhd + (a_i1_adhd*b_i1_anx)
             total_s1_anx_adhd := c_anx_adhd + (a_s1_adhd*b_s1_anx)
             total_i1_inat_adhd := c_inat_adhd + (a_i1_adhd*b_i1_inat)
             total_s1_inat_adhd := c_inat_adhd + (a_s1_adhd*b_s1_inat)
             total_i1_hyp_adhd := c_hyp_adhd + (a_i1_adhd*b_i1_hyp)
             total_s1_hyp_adhd := c_hyp_adhd + (a_s1_adhd*b_s1_hyp)
             total_i1_cd_adhd := c_cd_adhd + (a_i1_adhd*b_i1_cd)
             total_s1_cd_adhd := c_cd_adhd + (a_s1_adhd*b_s1_cd)
             total_i1_odd_adhd := c_odd_adhd + (a_i1_adhd*b_i1_odd)
             total_s1_odd_adhd := c_odd_adhd + (a_s1_adhd*b_s1_odd)
             
             total_i1_dep_asd := c_dep_asd + (a_i1_asd*b_i1_dep)
             total_s1_dep_asd := c_dep_asd + (a_s1_asd*b_s1_dep)
             total_i1_anx_asd := c_anx_asd + (a_i1_asd*b_i1_anx)
             total_s1_anx_asd := c_anx_asd + (a_s1_asd*b_s1_anx)
             total_i1_inat_asd := c_inat_asd + (a_i1_asd*b_i1_inat)
             total_s1_inat_asd := c_inat_asd + (a_s1_asd*b_s1_inat)
             total_i1_hyp_asd := c_hyp_asd + (a_i1_asd*b_i1_hyp)
             total_s1_hyp_asd := c_hyp_asd + (a_s1_asd*b_s1_hyp)
             total_i1_cd_asd := c_cd_asd + (a_i1_asd*b_i1_cd)
             total_s1_cd_asd := c_cd_asd + (a_s1_asd*b_s1_cd)
             total_i1_odd_asd := c_odd_asd + (a_i1_asd*b_i1_odd)
             total_s1_odd_asd := c_odd_asd + (a_s1_asd*b_s1_odd)
             
             total_i1_dep_scz := c_dep_scz + (a_i1_scz*b_i1_dep)
             total_s1_dep_scz := c_dep_scz + (a_s1_scz*b_s1_dep)
             total_i1_anx_scz := c_anx_scz + (a_i1_scz*b_i1_anx)
             total_s1_anx_scz := c_anx_scz + (a_s1_scz*b_s1_anx)
             total_i1_inat_scz := c_inat_scz + (a_i1_scz*b_i1_inat)
             total_s1_inat_scz := c_inat_scz + (a_s1_scz*b_s1_inat)
             total_i1_hyp_scz := c_hyp_scz + (a_i1_scz*b_i1_hyp)
             total_s1_hyp_scz := c_hyp_scz + (a_s1_scz*b_s1_hyp)
             total_i1_cd_scz := c_cd_scz + (a_i1_scz*b_i1_cd)
             total_s1_cd_scz := c_cd_scz + (a_s1_scz*b_s1_cd)
             total_i1_odd_scz := c_odd_scz + (a_i1_scz*b_i1_odd)
             total_s1_odd_scz := c_odd_scz + (a_s1_scz*b_s1_odd)
             
             total_i1_dep_bip := c_dep_bip + (a_i1_bip*b_i1_dep)
             total_s1_dep_bip := c_dep_bip + (a_s1_bip*b_s1_dep)
             total_i1_anx_bip := c_anx_bip + (a_i1_bip*b_i1_anx)
             total_s1_anx_bip := c_anx_bip + (a_s1_bip*b_s1_anx)
             total_i1_inat_bip := c_inat_bip + (a_i1_bip*b_i1_inat)
             total_s1_inat_bip := c_inat_bip + (a_s1_bip*b_s1_inat)
             total_i1_hyp_bip := c_hyp_bip + (a_i1_bip*b_i1_hyp)
             total_s1_hyp_bip := c_hyp_bip + (a_s1_bip*b_s1_hyp)
             total_i1_cd_bip := c_cd_bip + (a_i1_bip*b_i1_cd)
             total_s1_cd_bip := c_cd_bip + (a_s1_bip*b_s1_cd)
             total_i1_odd_bip := c_odd_bip + (a_i1_bip*b_i1_odd)
             total_s1_odd_bip := c_odd_bip + (a_s1_bip*b_s1_odd)
             
             total_i1_dep_mdd := c_dep_mdd + (a_i1_mdd*b_i1_dep)
             total_s1_dep_mdd := c_dep_mdd + (a_s1_mdd*b_s1_dep)
             total_i1_anx_mdd := c_anx_mdd + (a_i1_mdd*b_i1_anx)
             total_s1_anx_mdd := c_anx_mdd + (a_s1_mdd*b_s1_anx)
             total_i1_inat_mdd := c_inat_mdd + (a_i1_mdd*b_i1_inat)
             total_s1_inat_mdd := c_inat_mdd + (a_s1_mdd*b_s1_inat)
             total_i1_hyp_mdd := c_hyp_mdd + (a_i1_mdd*b_i1_hyp)
             total_s1_hyp_mdd := c_hyp_mdd + (a_s1_mdd*b_s1_hyp)
             total_i1_cd_mdd := c_cd_mdd + (a_i1_mdd*b_i1_cd)
             total_s1_cd_mdd := c_cd_mdd + (a_s1_mdd*b_s1_cd)
             total_i1_odd_mdd := c_odd_mdd + (a_i1_mdd*b_i1_odd)
             total_s1_odd_mdd := c_odd_mdd + (a_s1_mdd*b_s1_odd)
             
             total_i1_dep_anx := c_dep_anx + (a_i1_anx*b_i1_dep)
             total_s1_dep_anx := c_dep_anx + (a_s1_anx*b_s1_dep)
             total_i1_anx_anx := c_anx_anx + (a_i1_anx*b_i1_anx)
             total_s1_anx_anx := c_anx_anx + (a_s1_anx*b_s1_anx)
             total_i1_inat_anx := c_inat_anx + (a_i1_anx*b_i1_inat)
             total_s1_inat_anx := c_inat_anx + (a_s1_anx*b_s1_inat)
             total_i1_hyp_anx := c_hyp_anx + (a_i1_anx*b_i1_hyp)
             total_s1_hyp_anx := c_hyp_anx + (a_s1_anx*b_s1_hyp)
             total_i1_cd_anx := c_cd_anx + (a_i1_anx*b_i1_cd)
             total_s1_cd_anx := c_cd_anx + (a_s1_anx*b_s1_cd)
             total_i1_odd_anx := c_odd_anx + (a_i1_anx*b_i1_odd)
             total_s1_odd_anx := c_odd_anx + (a_s1_anx*b_s1_odd)
             
             total_i1_dep_alc := c_dep_alc + (a_i1_alc*b_i1_dep)
             total_s1_dep_alc := c_dep_alc + (a_s1_alc*b_s1_dep)
             total_i1_anx_alc := c_anx_alc + (a_i1_alc*b_i1_anx)
             total_s1_anx_alc := c_anx_alc + (a_s1_alc*b_s1_anx)
             total_i1_inat_alc := c_inat_alc + (a_i1_alc*b_i1_inat)
             total_s1_inat_alc := c_inat_alc + (a_s1_alc*b_s1_inat)
             total_i1_hyp_alc := c_hyp_alc + (a_i1_alc*b_i1_hyp)
             total_s1_hyp_alc := c_hyp_alc + (a_s1_alc*b_s1_hyp)
             total_i1_cd_alc := c_cd_alc + (a_i1_alc*b_i1_cd)
             total_s1_cd_alc := c_cd_alc + (a_s1_alc*b_s1_cd)
             total_i1_odd_alc := c_odd_alc + (a_i1_alc*b_i1_odd)
             total_s1_odd_alc := c_odd_alc + (a_s1_alc*b_s1_odd)
             
             total_i1_dep_ptsd := c_dep_ptsd + (a_i1_ptsd*b_i1_dep)
             total_s1_dep_ptsd := c_dep_ptsd + (a_s1_ptsd*b_s1_dep)
             total_i1_anx_ptsd := c_anx_ptsd + (a_i1_ptsd*b_i1_anx)
             total_s1_anx_ptsd := c_anx_ptsd + (a_s1_ptsd*b_s1_anx)
             total_i1_inat_ptsd := c_inat_ptsd + (a_i1_ptsd*b_i1_inat)
             total_s1_inat_ptsd := c_inat_ptsd + (a_s1_ptsd*b_s1_inat)
             total_i1_hyp_ptsd := c_hyp_ptsd + (a_i1_ptsd*b_i1_hyp)
             total_s1_hyp_ptsd := c_hyp_ptsd + (a_s1_ptsd*b_s1_hyp)
             total_i1_cd_ptsd := c_cd_ptsd + (a_i1_ptsd*b_i1_cd)
             total_s1_cd_ptsd := c_cd_ptsd + (a_s1_ptsd*b_s1_cd)
             total_i1_odd_ptsd := c_odd_ptsd + (a_i1_ptsd*b_i1_odd)
             total_s1_odd_ptsd := c_odd_ptsd + (a_s1_ptsd*b_s1_odd)
             
             total_i1_dep_ocd := c_dep_ocd + (a_i1_ocd*b_i1_dep)
             total_s1_dep_ocd := c_dep_ocd + (a_s1_ocd*b_s1_dep)
             total_i1_anx_ocd := c_anx_ocd + (a_i1_ocd*b_i1_anx)
             total_s1_anx_ocd := c_anx_ocd + (a_s1_ocd*b_s1_anx)
             total_i1_inat_ocd := c_inat_ocd + (a_i1_ocd*b_i1_inat)
             total_s1_inat_ocd := c_inat_ocd + (a_s1_ocd*b_s1_inat)
             total_i1_hyp_ocd := c_hyp_ocd + (a_i1_ocd*b_i1_hyp)
             total_s1_hyp_ocd := c_hyp_ocd + (a_s1_ocd*b_s1_hyp)
             total_i1_cd_ocd := c_cd_ocd + (a_i1_ocd*b_i1_cd)
             total_s1_cd_ocd := c_cd_ocd + (a_s1_ocd*b_s1_cd)
             total_i1_odd_ocd := c_odd_ocd + (a_i1_ocd*b_i1_odd)
             total_s1_odd_ocd := c_odd_ocd + (a_s1_ocd*b_s1_odd)
             
             total_i1_dep_an := c_dep_an + (a_i1_an*b_i1_dep)
             total_s1_dep_an := c_dep_an + (a_s1_an*b_s1_dep)
             total_i1_anx_an := c_anx_an + (a_i1_an*b_i1_anx)
             total_s1_anx_an := c_anx_an + (a_s1_an*b_s1_anx)
             total_i1_inat_an := c_inat_an + (a_i1_an*b_i1_inat)
             total_s1_inat_an := c_inat_an + (a_s1_an*b_s1_inat)
             total_i1_hyp_an := c_hyp_an + (a_i1_an*b_i1_hyp)
             total_s1_hyp_an := c_hyp_an + (a_s1_an*b_s1_hyp)
             total_i1_cd_an := c_cd_an + (a_i1_an*b_i1_cd)
             total_s1_cd_an := c_cd_an + (a_s1_an*b_s1_cd)
             total_i1_odd_an := c_odd_an + (a_i1_an*b_i1_odd)
             total_s1_odd_an := c_odd_an + (a_s1_an*b_s1_odd)
             
             total_i1_dep_ts := c_dep_ts + (a_i1_ts*b_i1_dep)
             total_s1_dep_ts := c_dep_ts + (a_s1_ts*b_s1_dep)
             total_i1_anx_ts := c_anx_ts + (a_i1_ts*b_i1_anx)
             total_s1_anx_ts := c_anx_ts + (a_s1_ts*b_s1_anx)
             total_i1_inat_ts := c_inat_ts + (a_i1_ts*b_i1_inat)
             total_s1_inat_ts := c_inat_ts + (a_s1_ts*b_s1_inat)
             total_i1_hyp_ts := c_hyp_ts + (a_i1_ts*b_i1_hyp)
             total_s1_hyp_ts := c_hyp_ts + (a_s1_ts*b_s1_hyp)
             total_i1_cd_ts := c_cd_ts + (a_i1_ts*b_i1_cd)
             total_s1_cd_ts := c_cd_ts + (a_s1_ts*b_s1_cd)
             total_i1_odd_ts := c_odd_ts + (a_i1_ts*b_i1_odd)
             total_s1_odd_ts := c_odd_ts + (a_s1_ts*b_s1_odd)
             
             total_i2_dep_adhd := c_dep_adhd + (a_i2_adhd*b_i2_dep)
             total_s2_dep_adhd := c_dep_adhd + (a_s2_adhd*b_s2_dep)
             total_i2_anx_adhd := c_anx_adhd + (a_i2_adhd*b_i2_anx)
             total_s2_anx_adhd := c_anx_adhd + (a_s2_adhd*b_s2_anx)
             total_i2_inat_adhd := c_inat_adhd + (a_i2_adhd*b_i2_inat)
             total_s2_inat_adhd := c_inat_adhd + (a_s2_adhd*b_s2_inat)
             total_i2_hyp_adhd := c_hyp_adhd + (a_i2_adhd*b_i2_hyp)
             total_s2_hyp_adhd := c_hyp_adhd + (a_s2_adhd*b_s2_hyp)
             total_i2_cd_adhd := c_cd_adhd + (a_i2_adhd*b_i2_cd)
             total_s2_cd_adhd := c_cd_adhd + (a_s2_adhd*b_s2_cd)
             total_i2_odd_adhd := c_odd_adhd + (a_i2_adhd*b_i2_odd)
             total_s2_odd_adhd := c_odd_adhd + (a_s2_adhd*b_s2_odd)
             
             total_i2_dep_asd := c_dep_asd + (a_i2_asd*b_i2_dep)
             total_s2_dep_asd := c_dep_asd + (a_s2_asd*b_s2_dep)
             total_i2_anx_asd := c_anx_asd + (a_i2_asd*b_i2_anx)
             total_s2_anx_asd := c_anx_asd + (a_s2_asd*b_s2_anx)
             total_i2_inat_asd := c_inat_asd + (a_i2_asd*b_i2_inat)
             total_s2_inat_asd := c_inat_asd + (a_s2_asd*b_s2_inat)
             total_i2_hyp_asd := c_hyp_asd + (a_i2_asd*b_i2_hyp)
             total_s2_hyp_asd := c_hyp_asd + (a_s2_asd*b_s2_hyp)
             total_i2_cd_asd := c_cd_asd + (a_i2_asd*b_i2_cd)
             total_s2_cd_asd := c_cd_asd + (a_s2_asd*b_s2_cd)
             total_i2_odd_asd := c_odd_asd + (a_i2_asd*b_i2_odd)
             total_s2_odd_asd := c_odd_asd + (a_s2_asd*b_s2_odd)
             
             total_i2_dep_scz := c_dep_scz + (a_i2_scz*b_i2_dep)
             total_s2_dep_scz := c_dep_scz + (a_s2_scz*b_s2_dep)
             total_i2_anx_scz := c_anx_scz + (a_i2_scz*b_i2_anx)
             total_s2_anx_scz := c_anx_scz + (a_s2_scz*b_s2_anx)
             total_i2_inat_scz := c_inat_scz + (a_i2_scz*b_i2_inat)
             total_s2_inat_scz := c_inat_scz + (a_s2_scz*b_s2_inat)
             total_i2_hyp_scz := c_hyp_scz + (a_i2_scz*b_i2_hyp)
             total_s2_hyp_scz := c_hyp_scz + (a_s2_scz*b_s2_hyp)
             total_i2_cd_scz := c_cd_scz + (a_i2_scz*b_i2_cd)
             total_s2_cd_scz := c_cd_scz + (a_s2_scz*b_s2_cd)
             total_i2_odd_scz := c_odd_scz + (a_i2_scz*b_i2_odd)
             total_s2_odd_scz := c_odd_scz + (a_s2_scz*b_s2_odd)
             
             total_i2_dep_bip := c_dep_bip + (a_i2_bip*b_i2_dep)
             total_s2_dep_bip := c_dep_bip + (a_s2_bip*b_s2_dep)
             total_i2_anx_bip := c_anx_bip + (a_i2_bip*b_i2_anx)
             total_s2_anx_bip := c_anx_bip + (a_s2_bip*b_s2_anx)
             total_i2_inat_bip := c_inat_bip + (a_i2_bip*b_i2_inat)
             total_s2_inat_bip := c_inat_bip + (a_s2_bip*b_s2_inat)
             total_i2_hyp_bip := c_hyp_bip + (a_i2_bip*b_i2_hyp)
             total_s2_hyp_bip := c_hyp_bip + (a_s2_bip*b_s2_hyp)
             total_i2_cd_bip := c_cd_bip + (a_i2_bip*b_i2_cd)
             total_s2_cd_bip := c_cd_bip + (a_s2_bip*b_s2_cd)
             total_i2_odd_bip := c_odd_bip + (a_i2_bip*b_i2_odd)
             total_s2_odd_bip := c_odd_bip + (a_s2_bip*b_s2_odd)
             
             total_i2_dep_mdd := c_dep_mdd + (a_i2_mdd*b_i2_dep)
             total_s2_dep_mdd := c_dep_mdd + (a_s2_mdd*b_s2_dep)
             total_i2_anx_mdd := c_anx_mdd + (a_i2_mdd*b_i2_anx)
             total_s2_anx_mdd := c_anx_mdd + (a_s2_mdd*b_s2_anx)
             total_i2_inat_mdd := c_inat_mdd + (a_i2_mdd*b_i2_inat)
             total_s2_inat_mdd := c_inat_mdd + (a_s2_mdd*b_s2_inat)
             total_i2_hyp_mdd := c_hyp_mdd + (a_i2_mdd*b_i2_hyp)
             total_s2_hyp_mdd := c_hyp_mdd + (a_s2_mdd*b_s2_hyp)
             total_i2_cd_mdd := c_cd_mdd + (a_i2_mdd*b_i2_cd)
             total_s2_cd_mdd := c_cd_mdd + (a_s2_mdd*b_s2_cd)
             total_i2_odd_mdd := c_odd_mdd + (a_i2_mdd*b_i2_odd)
             total_s2_odd_mdd := c_odd_mdd + (a_s2_mdd*b_s2_odd)
             
             total_i2_dep_anx := c_dep_anx + (a_i2_anx*b_i2_dep)
             total_s2_dep_anx := c_dep_anx + (a_s2_anx*b_s2_dep)
             total_i2_anx_anx := c_anx_anx + (a_i2_anx*b_i2_anx)
             total_s2_anx_anx := c_anx_anx + (a_s2_anx*b_s2_anx)
             total_i2_inat_anx := c_inat_anx + (a_i2_anx*b_i2_inat)
             total_s2_inat_anx := c_inat_anx + (a_s2_anx*b_s2_inat)
             total_i2_hyp_anx := c_hyp_anx + (a_i2_anx*b_i2_hyp)
             total_s2_hyp_anx := c_hyp_anx + (a_s2_anx*b_s2_hyp)
             total_i2_cd_anx := c_cd_anx + (a_i2_anx*b_i2_cd)
             total_s2_cd_anx := c_cd_anx + (a_s2_anx*b_s2_cd)
             total_i2_odd_anx := c_odd_anx + (a_i2_anx*b_i2_odd)
             total_s2_odd_anx := c_odd_anx + (a_s2_anx*b_s2_odd)
             
             total_i2_dep_alc := c_dep_alc + (a_i2_alc*b_i2_dep)
             total_s2_dep_alc := c_dep_alc + (a_s2_alc*b_s2_dep)
             total_i2_anx_alc := c_anx_alc + (a_i2_alc*b_i2_anx)
             total_s2_anx_alc := c_anx_alc + (a_s2_alc*b_s2_anx)
             total_i2_inat_alc := c_inat_alc + (a_i2_alc*b_i2_inat)
             total_s2_inat_alc := c_inat_alc + (a_s2_alc*b_s2_inat)
             total_i2_hyp_alc := c_hyp_alc + (a_i2_alc*b_i2_hyp)
             total_s2_hyp_alc := c_hyp_alc + (a_s2_alc*b_s2_hyp)
             total_i2_cd_alc := c_cd_alc + (a_i2_alc*b_i2_cd)
             total_s2_cd_alc := c_cd_alc + (a_s2_alc*b_s2_cd)
             total_i2_odd_alc := c_odd_alc + (a_i2_alc*b_i2_odd)
             total_s2_odd_alc := c_odd_alc + (a_s2_alc*b_s2_odd)
             
             total_i2_dep_ptsd := c_dep_ptsd + (a_i2_ptsd*b_i2_dep)
             total_s2_dep_ptsd := c_dep_ptsd + (a_s2_ptsd*b_s2_dep)
             total_i2_anx_ptsd := c_anx_ptsd + (a_i2_ptsd*b_i2_anx)
             total_s2_anx_ptsd := c_anx_ptsd + (a_s2_ptsd*b_s2_anx)
             total_i2_inat_ptsd := c_inat_ptsd + (a_i2_ptsd*b_i2_inat)
             total_s2_inat_ptsd := c_inat_ptsd + (a_s2_ptsd*b_s2_inat)
             total_i2_hyp_ptsd := c_hyp_ptsd + (a_i2_ptsd*b_i2_hyp)
             total_s2_hyp_ptsd := c_hyp_ptsd + (a_s2_ptsd*b_s2_hyp)
             total_i2_cd_ptsd := c_cd_ptsd + (a_i2_ptsd*b_i2_cd)
             total_s2_cd_ptsd := c_cd_ptsd + (a_s2_ptsd*b_s2_cd)
             total_i2_odd_ptsd := c_odd_ptsd + (a_i2_ptsd*b_i2_odd)
             total_s2_odd_ptsd := c_odd_ptsd + (a_s2_ptsd*b_s2_odd)
             
             total_i2_dep_ocd := c_dep_ocd + (a_i2_ocd*b_i2_dep)
             total_s2_dep_ocd := c_dep_ocd + (a_s2_ocd*b_s2_dep)
             total_i2_anx_ocd := c_anx_ocd + (a_i2_ocd*b_i2_anx)
             total_s2_anx_ocd := c_anx_ocd + (a_s2_ocd*b_s2_anx)
             total_i2_inat_ocd := c_inat_ocd + (a_i2_ocd*b_i2_inat)
             total_s2_inat_ocd := c_inat_ocd + (a_s2_ocd*b_s2_inat)
             total_i2_hyp_ocd := c_hyp_ocd + (a_i2_ocd*b_i2_hyp)
             total_s2_hyp_ocd := c_hyp_ocd + (a_s2_ocd*b_s2_hyp)
             total_i2_cd_ocd := c_cd_ocd + (a_i2_ocd*b_i2_cd)
             total_s2_cd_ocd := c_cd_ocd + (a_s2_ocd*b_s2_cd)
             total_i2_odd_ocd := c_odd_ocd + (a_i2_ocd*b_i2_odd)
             total_s2_odd_ocd := c_odd_ocd + (a_s2_ocd*b_s2_odd)
             
             total_i2_dep_an := c_dep_an + (a_i2_an*b_i2_dep)
             total_s2_dep_an := c_dep_an + (a_s2_an*b_s2_dep)
             total_i2_anx_an := c_anx_an + (a_i2_an*b_i2_anx)
             total_s2_anx_an := c_anx_an + (a_s2_an*b_s2_anx)
             total_i2_inat_an := c_inat_an + (a_i2_an*b_i2_inat)
             total_s2_inat_an := c_inat_an + (a_s2_an*b_s2_inat)
             total_i2_hyp_an := c_hyp_an + (a_i2_an*b_i2_hyp)
             total_s2_hyp_an := c_hyp_an + (a_s2_an*b_s2_hyp)
             total_i2_cd_an := c_cd_an + (a_i2_an*b_i2_cd)
             total_s2_cd_an := c_cd_an + (a_s2_an*b_s2_cd)
             total_i2_odd_an := c_odd_an + (a_i2_an*b_i2_odd)
             total_s2_odd_an := c_odd_an + (a_s2_an*b_s2_odd)
             
             total_i2_dep_ts := c_dep_ts + (a_i2_ts*b_i2_dep)
             total_s2_dep_ts := c_dep_ts + (a_s2_ts*b_s2_dep)
             total_i2_anx_ts := c_anx_ts + (a_i2_ts*b_i2_anx)
             total_s2_anx_ts := c_anx_ts + (a_s2_ts*b_s2_anx)
             total_i2_inat_ts := c_inat_ts + (a_i2_ts*b_i2_inat)
             total_s2_inat_ts := c_inat_ts + (a_s2_ts*b_s2_inat)
             total_i2_hyp_ts := c_hyp_ts + (a_i2_ts*b_i2_hyp)
             total_s2_hyp_ts := c_hyp_ts + (a_s2_ts*b_s2_hyp)
             total_i2_cd_ts := c_cd_ts + (a_i2_ts*b_i2_cd)
             total_s2_cd_ts := c_cd_ts + (a_s2_ts*b_s2_cd)
             total_i2_odd_ts := c_odd_ts + (a_i2_ts*b_i2_odd)
             total_s2_odd_ts := c_odd_ts + (a_s2_ts*b_s2_odd)
             
             '
