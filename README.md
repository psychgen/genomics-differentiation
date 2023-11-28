# genomics-differentiation
Data preparation and analysis code for the paper "The genetic architecture of differentiating behavioral and emotional problems in early life"

Authors: Adrian Dahl Askelund (jaaskelu@uio.no / ada@lds.no), Laura Hegemann, Andrea G. Allegrini, Elizabeth C. Corfield, Helga Ask, Neil M. Davies, Ole A. Andreassen, Alexandra Havdahl, & Laurie J. Hannigan (laurie.hannigan@bristol.ac.uk).

00_create_polygenic_scores.R creates polygenic scores (PGS) for mothers, fathers, and children, which are merged with the phenotypic data in the script ‘01_data_preparation.R’.
00.1_create_mdd_pgs.R creates a new MDD PGS.

01_data_preparation.R curates and selects the analytic dataset based on questionnaire, covariate, genotype, and registry data, including data manipulation and preparation of variables. It then creates an illustration of how differentiation is operationalised based on a subset of the raw data.

02_wave_specific_GWAS.R conducts wave-specific GWAS of the difference and total scores in Regenie at the ages of 1.5, 3, and 5 years. Additional preparation steps are conducted by the scripts 02.1-02.3.

02.1_prepare_sumstats.R prepares the summary statistics for multivariate GWAS in genomic SEM.

02.2_munge_sumstats.R munges the summary statistics before running ld-score regression.

02.3_ldsc.R runs LDSC on the summary statistics for running multivariate GWAS.

02.4_run_multivariate_GWAS_diff.R runs multivariate GWAS of the differentiation slope and intercept from an LGM that is equivalent to the phenotypic LGM, but specified based on a genetic variance/covariance matrix. 

02.5_run_multivariate_GWAS_tot.R runs multivariate GWAS of the total problem slope and intercept from the same model as in 02.4, with differentiation and total constrained to be orthogonal. 

02.6_predicted_sample_size_GWAS.R checks the output from the multivariate GWAS and calculates the predicted sample size for the intercept and slope factors.

02.7_plot_results_GWAS.R creates miami plots, manhattan plots, and QQ plots based on the multivariate GWAS output.

03_genetic_correlations.R simply states that genetic correlations of differentiation and total problems with the 11 psychiatric and neurodevelopmental conditions are estimated outside of R, in LDSC.

03.1_calculate_Neff_sumstats.R calculates the sample size (N) as the sum of effective sample sizes for each of the 11 GWAS meta-analyses. 

03.2_prepare_sumstats.R prepares the summary statistics for the 11 psychiatric conditions for genomic SEM. 

03.3_run_ldsc_wpsych.R runs ld-score regression of differentiation and total problems, and the 11 psychiatric conditions. 

03.4_plot_genetic_correlations.R plots genetic correlations of the 11 psychiatric and neurodevelopmental conditions with differentiation and total problems.

04_run_structural_models.R runs the correlated factor, hierarchical, and bifactor models with factors predicting differentiation and total, by sourcing the script '04.1_specify_structural_models.R'. 

04.1_specify_structural_models.R specifies a basic LGM and three (previously established) structural models accounting for variance across 11 psychiatric conditions, which are run by the script '04_run_structural_models.R'.

04.2_plot_structural_models.R creates plots of the structural models of liability to 11 psychiatric conditions, and resulting path estimates.

05_run_combined_LGMs_with_pgs.R runs the LGMs with PGS by sourcing the script ‘05.1_specify_combined_LGMs_with_pgs.R’.

05.1_specify_combined_LGMs_with_pgs.R specifies LGMs with 11 PGS as predictors.

06_run_trio_pgs_analyses.R runs the LGMs with trio PGS by sourcing the script ‘06.1_specify_LGMs_with_trio_pgs.R’.

06.1_specify_LGMs_with_trio_pgs.R specifies LGMs with trio PGS as predictors.

07_run_equiv_tests.R runs equivalence tests to determine, in conjunction with null hypothesis significance tests, whether the PGS effects are null, non-null, or undecided.
