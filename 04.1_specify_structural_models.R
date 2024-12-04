#04.1_specify_structural_models.R

#This script specifies basic LGMs and three (previously established) structural 
#models accounting for variance across 11 psychiatric and neurodevelopmental
#conditions, including differentiation and total problems as predictors. 
#The models are run by the script: '04_run_structural_models.R'.

## 1. basic latent growth models (LGMs)
## 2. correlated factor models
## 3. hierarhical models
## 4. bifactor models

#basic LGM

model_basic <-
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

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
           '

#basic LGM with no slope

model_no_slope <-
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           
# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           i2 ~~ i2
           i1 ~~ i2
           '

#lgm diff with correlated factors as outcomes

modelD_4factors <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc

# outcomes (effects)

           com + psy + neu + int ~ i
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           '

#lgm diff with correlated factors as outcomes
#but with paths to indicators instead of "neu" factor

modelD_4factors_ind <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc

# outcomes (effects)

           com + psy + ts + alc + adhd + asd + ptsd + mdd + int ~ i
           neu ~ 0*i
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           b > .001
           '


#lgm tot with correlated factors as outcomes

modelT_4factors <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i ~~ i

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc

# outcomes (effects)

           com + psy + neu + int ~ i
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           '

#lgm tot + diff with correlated factors as outcomes

modelDT_4factors <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           i2 ~~ i2
           i1 ~~ i2

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc

# outcomes (effects)

           com + psy + neu + int ~ i1 + i2
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
# outcomes (variance)

           com ~~ com
           psy ~~ psy
           neu ~~ neu
           int ~~ int
           '


#lgm diff w/p-factor from hierarchical model as the outcome

modelD_hier_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc
           p =~ com + psy + neu + int

# outcomes (effects)

           p ~ i
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ a*anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           '

#lgm diff w/sub-factors from hierarchical model as outcomes

modelD_hi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i
           p ~~ 0*i
           
# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc
           p =~ com + psy + neu + int

# outcomes (effects)

           com + psy + neu + int ~ i
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ a*anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           
# outcomes (variances)         
           
           com ~~ com
           psy ~~ psy
           int ~~ int
           neu ~~ b*neu
           
           b > .001
           '

#lgm of diff + tot w/sub-factors from hierarchical model as outcomes

modelDT_hi_sub <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           i1 ~~ 0*p
           i2 ~~ i2
           i2 ~~ 0*p
           i1 ~~ i2

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc
           p =~ com + psy + neu + int

# outcomes (effects)

           com + psy + neu + int ~ i1 + i2
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
# outcomes (variances)
           
           com ~~ a*com
           psy ~~ psy
           int ~~ int
           neu ~~ b*neu
           
           a > .001
           b > .001
           '

#lgm of diff + tot w/p-factor from hierarchical model as the outcome

modelDT_hi_p <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           i2 ~~ i2
           i1 ~~ i2

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc
           p =~ com + psy + neu + int

# outcomes (effects)

           p ~ i1 + i2
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ a*anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           '

#lgm tot w/p-factor from hierarchical model as the outcome

modelT_hi_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i ~~ i

# outcomes (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc
           p =~ com + psy + neu + int

# outcomes (effects)

           p ~ i
                   
# indicators (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ a*anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           
# outcomes (variance)

           com ~~ com
           psy ~~ psy
           neu ~~ b*neu
           int ~~ int
           
           b > .001
           '

#lgm tot w/sub-factors from hierarchical model as outcomes

modelT_hi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i ~~ i
           p ~~ 0*i

# predictors (latent variables)

           com =~ ocd + an + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ anx + mdd + ptsd + alc
           p =~ com + psy + neu + int

# outcomes (effects)

           com + psy + neu + int ~ i
                   
# outcomes (variance)

           adhd ~~ adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ alc
           ptsd ~~ ptsd
           ocd ~~ ocd
           an ~~ an
           ts ~~ ts
           
# outcomes (variance)

           com ~~ com
           psy ~~ psy
           neu ~~ a*neu
           int ~~ int
           
           a > .001
           '

#lgm diff with p from the bifactor model as the outcome

modelD_bi_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i
           i ~~ 0*com
           i ~~ 0*psy
           i ~~ 0*neu
           i ~~ 0*int

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + ts + mdd
           int =~ mdd + ptsd + anx + alc

# outcomes (covariances)

           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomes (effects)

           p ~ i
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           b > .001
           c > .001
           '

#lgm diff w/sub-factors from the bifactor model as outcomes

modelD_bi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i
           p ~~ 0*i

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + mdd + ts
           int =~ mdd + ptsd + anx + alc

# outcomes (covariances)

           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomes (effects)

           com + psy + neu + int ~ i
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           b > .001
           c > .001
           
# outcomes (variances)
           
           com ~~ com
           psy ~~ psy
           neu ~~ d*neu
           int ~~ int
           
           d > .001
           '

#lgm diff w/both p and the sub-factors from the bifactor model as outcomes

modelD_bi_all <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3

# growth parameter (co)variances
    
           i ~~ i

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + mdd + ts
           int =~ ptsd + anx + mdd + alc

# outcomes (covariances)

           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomes (effects)

           p + com + psy + neu + int ~ i
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           b > .001
           c > .001
           
# outcomes (variances)
           
           p ~~ p
           com ~~ com
           psy ~~ psy
           neu ~~ d*neu
           int ~~ int
           
           d > .001
           '


#lgm tot with p from the bifactor model as the outcome

modelT_bi_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i ~~ i
           i ~~ 0*com
           i ~~ 0*psy
           i ~~ 0*neu
           i ~~ 0*int

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ bip + scz + alc
           neu =~ adhd + asd + alc + ptsd + mdd + ts
           int =~ anx + mdd + ptsd + alc

# outcomes (covariances)
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomes (effects)

           p ~ i
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           b > .001
           c > .001
           '

#lgm tot w/sub-factors from the bifactor model as outcomes

modelT_bi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i ~~ i
           p ~~ 0*i

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + mdd + ts
           int =~ anx + mdd + ptsd + alc

# outcomes (covariances)
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomess (effects)

           com + psy + neu + int ~ i
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ d*ts
           
           a > .001
           b > .001
           c > .001
           d > .001
           '


#lgm diff+tot with p from the bifactor model as the outcome

modelDT_bi_p <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3

# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           i2 ~~ i2
           i1 ~~ i2

           i1 + i2 ~~ 0*com
           i1 + i2 ~~ 0*psy
           i1 + i2 ~~ 0*neu
           i1 + i2 ~~ 0*int

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + mdd + ts
           int =~ mdd + ptsd + anx + alc

# outcomes (covariances)
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomes (effects)

           p ~ i1 + i2
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ ts
           
           a > .001
           b > .001
           c > .001
           '

#lgm diff+tot w/ p and sub-factors from the bifactor model as outcomes

modelDT_bi_all <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           
# observed variable variances

           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
           
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           i2 ~~ i2
           i1 ~~ i2
           
           p ~~ 0*i1
           p ~~ 0*i2

# outcomes (latent variables)

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + alc + ptsd + mdd + ts
           int =~ mdd + ptsd + anx + alc

# outcomes (covariances)
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# outcomess (effects)

           p + com + psy + neu + int ~ i1 + i2
                   
# indicators (variance)

           adhd ~~ a*adhd
           asd ~~ asd
           scz ~~ scz
           bip ~~ bip
           mdd ~~ mdd
           anx ~~ anx
           alc ~~ b*alc
           ptsd ~~ ptsd
           ocd ~~ c*ocd
           an ~~ an
           ts ~~ d*ts
           
           a > .001
           b > .001
           c > .001
           d > .001
           '
