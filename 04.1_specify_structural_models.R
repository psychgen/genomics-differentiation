#04.1_specify_structural_models.R

#this script specifies a basic LGM and three (previously established) 
#structural models accounting for variance across 11 psychiatric 
#conditions, which are run by the script: '04_run_structural_models.R'

## 1. basic latent growth model (LGM)
## 2. correlated factor models
## 3. hierarhical models
## 4. bifactor models

#basic LGM with no predictors

model_basic <-
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s
           '

#lgm diff with correlated factors as predictors

modelD_4factors <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + psy + neu + int
           s ~ com + psy + neu + int
                   
# predictors (variance)

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

#lgm diff with correlated factors as predictors, except for 
#compulsive indicators (independent pathway model)

modelD_4factors_com <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ psy + neu + int + an + ocd + ts
           s ~ psy + neu + int + an + ocd + ts
                   
# predictors (variance)

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

#lgm diff with correlated factors as predictors, except for 
#psychotic indicators (independent pathway model)

modelD_4factors_psy <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + neu + int + scz + bip + alc
           s ~ com + neu + int + scz + bip + alc
                   
# predictors (variance)

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

#lgm diff with correlated factors as predictors, except for 
#neurodevelopmental indicators (independent pathway model)

modelD_4factors_neu <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + psy + int + adhd + asd + ptsd + alc + mdd + ts
           s ~ com + psy + int + adhd + asd + ptsd + alc + mdd + ts
                   
# predictors (variance)

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

#lgm diff with correlated factors as predictors, except for 
#internalising indicators (independent pathway model)

modelD_4factors_int <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + psy + neu + ptsd + mdd + anx + alc
           s ~ com + psy + neu + ptsd + mdd + anx + alc
                   
# predictors (variance)

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

#lgm tot with correlated factors as predictors

modelT_4factors <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + psy + neu + int
           s ~ com + psy + neu + int
                   
# predictors (variance)

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

#lgm tot with correlated factors as predictors, except for 
#compulsive indicators (independent pathway model)

modelT_4factors_com <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ psy + neu + int + an + ocd + ts
           s ~ psy + neu + int + an + ocd + ts
                   
# predictors (variance)

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

#lgm tot with correlated factors as predictors, except for 
#psychotic indicators (independent pathway model)

modelT_4factors_psy <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + neu + int + scz + bip + alc
           s ~ com + neu + int + scz + bip + alc
                   
# predictors (variance)

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

#lgm tot with correlated factors as predictors, except for 
#neurodevelopmental indicators (independent pathway model)

modelT_4factors_neu <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + psy + int + adhd + asd + ptsd + alc + mdd + ts
           s ~ com + psy + int + adhd + asd + ptsd + alc + mdd + ts
                   
# predictors (variance)

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

#lgm tot with correlated factors as predictors, except for 
#internalising indicators (independent pathway model)

modelT_4factors_int <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc

# predictors (effects)

           i ~ com + psy + neu + ptsd + mdd + anx + alc
           s ~ com + psy + neu + ptsd + mdd + anx + alc
                   
# predictors (variance)

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

#lgm diff w/p-factor from hierarchical model as predictor

modelD_hier_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           p =~ com + psy + neu + int

# predictors (effects)

           i ~ p
           s ~ p
                   
# predictors (variance)

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

#lgm diff w/sub-factors from hierarchical model as predictors

modelD_hi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           p =~ com + psy + neu + int

# predictors (effects)

           i ~ com + psy + neu + int
           s ~ com + psy + neu + int
                   
# predictors (variance)

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

#lgm of diff + tot w/sub-factors from hierarchical model as predictors

modelDT_hi_sub <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3
           
           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i1 ~~ a*i1
           s1 ~~ b*s1
           i1 ~~ s1
           
           a > .001
           b > .001
           
           i2 ~~ c*i2
           s2 ~~ d*s2
           i2 ~~ s2
           
           c > .001
           d > .001
           
           i1 ~~ i2
           s1 ~~ s2
           i1 ~~ s2
           s1 ~~ i2

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           p =~ com + psy + neu + int

# predictors (effects)

           i1 + i2 ~ com + psy + neu + int
           s1 + s2 ~ com + psy + neu + int
                   
# predictors (variance)

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

#lgm of diff + tot w/p-factor from hierarchical model as predictor

modelDT_hi_p <-   
           '
# growth parameters (latent variables)
  
           i1 =~ 1*diff1 + 1*diff2 + 1*diff3
           s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
           i2 =~ 1*tot1 + 1*tot2 + 1*tot3
           s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3
           
           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i1 ~~ i1
           s1 ~~ s1
           i1 ~~ s1
           
           i2 ~~ i2
           s2 ~~ s2
           i2 ~~ s2
           
           i1 ~~ i2
           s1 ~~ s2
           i1 ~~ s2
           s1 ~~ i2

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           p =~ com + psy + neu + int

# predictors (effects)

           i1 + i2 ~ p
           s1 + s2 ~ p
                   
# predictors (variance)

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

#lgm tot w/p-factor from hierarchical model as predictor

modelT_hi_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           p =~ com + psy + neu + int

# predictors (effects)

           i ~ p
           s ~ p
                   
# predictors (variance)

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

#lgm tot w/sub-factors from hierarchical model as predictors

modelT_hi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ a*i
           s ~~ b*s
           i ~~ s
           
           a > .001
           b > .001

           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           p =~ com + psy + neu + int

# predictors (effects)

           i ~ com + psy + neu + int
           s ~ com + psy + neu + int
                   
# predictors (variance)

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

#lgm diff with p from the bifactor model as predictor

modelD_bi_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# predictors (effects)

           i ~ p
           s ~ p
                   
# predictors (variance)

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

#lgm diff w/sub-factors from the bifactor model as predictors

modelD_bi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
           
# observed variable variances

           diff1 ~~ 0*diff1
           diff2 ~~ 0*diff2
           diff3 ~~ 0*diff3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# predictors (effects)

           i ~ com + psy + neu + int
           s ~ com + psy + neu + int
                   
# predictors (variance)

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

#lgm tot with p from the bifactor model as predictor

modelT_bi_p <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# predictors (effects)

           i ~ p
           s ~ p
                   
# predictors (variance)

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

#lgm tot w/sub-factors from the bifactor model as predictors

modelT_bi_sub <-   
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
           
# observed variable variances

           tot1 ~~ 0*tot1
           tot2 ~~ 0*tot2
           tot3 ~~ 0*tot3

# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s

           p =~ an + ocd + ts + scz + bip + alc + adhd + asd + ptsd + mdd + anx
           com =~ an + ocd + ts
           psy =~ scz + bip + alc
           neu =~ adhd + asd + ptsd + alc + mdd + ts
           int =~ ptsd + mdd + anx + alc
           
           p ~~ 0*com + 0*psy + 0*neu + 0*int
           com ~~ 0*psy + 0*neu + 0*int
           psy ~~ 0*neu + 0*int
           neu ~~ 0*int

# predictors (effects)

           i ~ com + psy + neu + int
           s ~ com + psy + neu + int
                   
# predictors (variance)

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
