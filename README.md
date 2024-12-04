Analysis code overview: genomics\_differentiation
================
Adrian Askelund (jo.adrian.dahl.askelund(at)fhi.no)

4 Dec 2024

-   [Introduction](#introduction)
-   [Data preparation](#data-preparation)
    -   [Creating polygenic scores](#creating-polygenic-scores)
    -   [Scale- and item-level data preparation](#scale-and-item-level-data-preparation)
-   [Analyses](#analyses)
    -   [Wave-specific GWAS](#wave-specific-gwas)
    -   [Multivariate GWAS](#multivariate-gwas)
    -   [Genetic correlations](#genetic-correlations)
    -   [Structural models](#structural-models)
    -   [Child PGS analyses](#child-pgs-analyses)
    -   [Trio-PGS analyses](#trio-pgs-analyses)
    -   [Equivalence testing](#equivalence-testing)

Introduction
============

This is an overview of the analysis code for the project: "The genetic architecture of differentiating behavioral and emotional problems in early life". Here, we walk through the scripts used to complete the analyses for this project.

Data preparation
================

There are six main data preparation/analysis steps in this project (with the scripts numbered accordingly):

1.  Data preparation: curating the analytic dataset based on questionnaire, covariate, genotype, and registry data
2.  Multivariate GWAS based on wave-specific GWAS of differentiation and total problems (at ages 1.5, 3, and 5 years)
3.  Genetic correlations of differentiation and total problems with 11 psychiatric and neurodevelopmental conditions
4.  Structural models: correlated factor, hierarchical, and bifactor models with differentiation/total predicting latent factors
5.  Latent growth models with child polygenic scores (PGS) included as predictors of differentiation and total problems
6.  Latent growth models with trio (mother, father, and child) PGS included as predictors of differentiation/total problems

Creating polygenic scores
-------------------------

First, polygenic scores (PGS) are created using LDpred2, run on a high performance computing cluster via a standard pipeline implemented in the 'genotools' R package. Necessary inputs were prepared using the scripts `00.1_calculate_Neff_sumstats.R` and `00.2_create_pgs_job_scripts.R`. The PGS are then fetched and processed (i.e., regressed on genotyping batch and 20 principal components) in R using the script `00_create_polygenic_scores.R`.

Scale- and item-level data preparation
---------------------------

To prepare data on the scale and item level, we use the `01_data_preparation.R` script. This script:

-   Curates the phenotypic dataset using the 'phenotools' R package, selecting and merging the relevant scale- and item-level variables
-   Does data manipulation (i.e., recoding the covariates) and creates the differentiation and total problem scores, using the following code:

``` r
# scale CBCL scores
pheno_data <- pheno_data %>% 
  mutate_at(vars(matches("int|ext")), list(~scale(.)))

# create and scale differentiation scores
pheno_data <- pheno_data %>%
  mutate(diff1 = scale(ext1 - int1)) %>%
  mutate(diff2 = scale(ext2 - int2)) %>%
  mutate(diff3 = scale(ext3 - int3))

# create and scale total scores
pheno_data <- pheno_data %>%
  mutate(tot1 = scale(ext1 + int1)) %>%
  mutate(tot2 = scale(ext2 + int2)) %>%
  mutate(tot3 = scale(ext3 + int3))
```

-   Selects the analytic sample, creates phenotype and covariate files for wave-specific GWAS, and merges the phenotype and genotype data. 
-   Creates an illustration of how the differentiation and total problem scores are created based on a random sub-sample of MoBa children. 

Analyses
========

Wave-specific GWAS
---------------------------------------------------

To facilitate running multivariate GWAS of differentiation and total problems, we conducted wave-specific GWAS of the difference and total scores in REGENIE at the ages of 1.5, 3, and 5. Scripts for conducting GWAS in REGENIE are in the 'shell_scripts/GWAS' folder. 

In the script `02_wave_specific_GWAS.R`, summary statistics from the wave-specific GWAS conducted by REGENIE are prepared for multivariate GWAS in GenomicSEM. Additional preparation steps are conducted by the scripts `02.1...`, `02.2...`, and `02.3...` for, respectively, 1) preparing, 2) munging, and 3) running LD-score regression on the summary statistics, enabling us to run the multivariate GWAS.

Multivariate GWAS
--------------------

Next we run a multivariate GWAS of the differentiation intercept from a latent growth model (LGM) that is equivalent to the phenotypic LGM, but specified based on the genetic covariance matrix generated above. The multivariate (i.e., longitudinal) GWAS is run by the script `02.4_run_multivariate_GWAS_diff.R` on the computing cluster using the script `02.4_run_multivariate_GWAS_diff.sh` in the 'shell_scripts/multivariate GWAS' folder. The multivariate GWAS for differentiation is specified and run using the following code:

``` r
load(file="/ess/p471/data/durable/projects/differentiation_genomics/cluster/scripts/GWAS/input_files/ldsc.RData")
load(file="/ess/p471/data/durable/projects/differentiation_genomics/cluster/scripts/GWAS/input_files/sumstats.RData")

lgm.model <-
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
           i1 ~~ 0*i2
           s1 ~~ 0*s2
           i1 ~~ 0*s2
           s1 ~~ 0*i2
           
# snp effects

           i1 ~ SNP
           '

sub<-c("i1 ~ SNP")

GWAS_both_diff <-userGWAS(covstruc=ldsc,SNPs=sumstats,model=lgm.model,estimation="DWLS",
                          sub=sub,smooth_check=FALSE,cores=20,parallel=TRUE)

save(GWAS_both_diff, file="GWAS_both_diff.RData")
```

We then run a multivariate GWAS of the total problem intercept from the same model as in script `02.4...`, with differentiation and total problems constrained to be uncorrelated (matching their relationship at the phenotypic level). The multivariate GWAS of total problems is run by the script `02.5_run_multivariate_GWAS_tot.R` on the cluster.

Based on the results of the multivariate GWAS, we calculate the predicted sample size for the latent intercept factors using the script `02.6_predicted_sample_size_GWAS.R`. This is necessary to calculate genetic correlations using LDSC. Miami plots and QQ plots based on the multivariate GWAS output are then generated using the script `02.7_plot_results_GWAS.R`. 

Genetic correlations
-----------------------
Genetic correlations of differentiation and total problems with the 11 psychiatric and neurodevelopmental conditions are estimated in LDSC using the scripts located in the folder 'shell_scripts/genetic correlations'. To enable running LDSC, the summary statistics from the multivariate GWAS of the differentiation and total problem intercepts are munged using the script `03.1_munge_multivar_sumstats.R`, which is run on the cluster using the `03.1_munge_multivar_sumstats.sh` script located in the 'shell_scripts/genetic correlations' folder.

We also munge the summary statistics for the 11 psychiatric and neurodevelopmental conditions before running LD-score regression, using the script `03.2_munge_sumstats_wpsych.R`, run on the cluster. In addition, we run LD-score regression of the differentiation and total problem scores together with the 11 psychiatric conditions using the script `03.3_run_ldsc_wpsych.R`, also run on the cluster. This is done using the following code:

```r
traits<- c("an.sumstats.gz","ocd.sumstats.gz","ts.sumstats.gz",
           "scz.sumstats.gz","bip.sumstats.gz","alc.sumstats.gz",
           "adhd.sumstats.gz","asd.sumstats.gz","ptsd.sumstats.gz",
           "mdd.sumstats.gz","anx.sumstats.gz",
           "diff1.sumstats.gz","tot1.sumstats.gz","diff2.sumstats.gz",
           "tot2.sumstats.gz","diff3.sumstats.gz","tot3.sumstats.gz")

sample.prev <- c(.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,NA,NA,NA,NA,NA,NA)

population.prev <- c(.009,.02,.007,.01,.01,.12,.087,.02,.068,.21,.31,NA,NA,NA,NA,NA,NA)

ld <- "/cluster/p/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/eur_w_ld_chr/"

wld <- "/cluster/p/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/eur_w_ld_chr/"

trait.names=c("an","ocd","ts","scz","bip","alc","adhd","asd","ptsd","mdd","anx",
              "diff1","tot1","diff2","tot2","diff3","tot3")

stand=TRUE

# run the ldsc function 
cov_wpsych <- ldsc(traits=traits,sample.prev=sample.prev,population.prev=population.prev, 
                   ld=ld,wld=wld,trait.names=trait.names,stand=stand)

save(cov_wpsych, file="cov_wpsych.RData")
```

Note that the sample and population prevalences are specified only for the binary traits. The sample prevalences are set to .5 here because the sum of effective sample sizes were provided in the previous munging step. At the end, the results are saved out to enable running the structural models in the next step of the analysis. 

Finally, we plot the genetic correlations of differentiation and total problems with the 11 psychiatric and neurodevelopmental conditions using the script `03.4_plot_genetic_correlations.R`.

Structural models
--------------------

Next, we run structural models of liability to 11 psychiatric and neurodevelopmental conditions with differentiation and total problems predicting the latent factors using the script `04_run_structural_models.R`. This script sources `04.1_specify_structural_models.R`, which specifies four types of models: 1) basic LGMs; 2) correlated factor models; 3) hierarchical models; and 4) bifactor models. The results are saved out and plotted in `04.2_plot_structural_models.R`. 

Child PGS analyses
-----------------------

The LGMs with child PGS included as predictors of differentiation and total problems are run using the script `05_run_combined_LGMs_with_pgs.R` by sourcing the script `05.1_specify_combined_LGMs_with_pgs.R`. This script specifies four models: 1) a basic LGM; 2) an LGM with child PGS as predictors (intercept and slope); 3) an LGM with effects on the intercepts only; and 4) an LGM with effects on the slopes only. The results are saved out for combining with the trio-PGS results in tables and figures. 

Trio-PGS analyses
-----------------------

The LGMs with trio PGS as predictors are run using the script `06_run_trio_pgs_analyses.R` by sourcing the script `06.1_specify_LGMs_with_trio_pgs.R`. This script specifies three models: 1) a multivariate PGS model in the full sample of trios; 2) a multivariate PGS model restricted to unrelated trios; and 3) univariate PGS models in the full sample of trios. To demonstrate, the univariate PGS models are specified using the following code: 

```r
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
```

The univariate models are then run with the following code (in `06_run_trio_pgs_analyses.R`): 

```r          
# loop to run univariate trio PGS models on full sample (with clustering on 
# maternal id to account for child siblings as the main source of relatedness)
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
```

After running the models, the script processes, plots, and tabulates the results from each of the models.

Equivalence testing
-----------------------

Finally, we run equivalence tests (using the script `07_run_equivalence_tests.R`) to determine, in conjunction with null hypothesis significance tests, whether the PGS effects are null, non-null, or undecided. The possible outcomes are:

a) null (within the region of practical equivalence to zero); 
b) non-null (statistically significant and not entirely within the region of practical equivalence to zero); or
c) undecided (not statistically significant and not entirely within the region of practical equivalence to zero). 
