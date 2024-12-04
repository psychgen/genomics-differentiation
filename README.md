# genomics-differentiation
Data preparation and analysis code for the paper "The genetic architecture of differentiating behavioral and emotional problems in early life"

Authors: Adrian Dahl Askelund (jo.adrian.dahl.askelund@fhi.no), Laura Hegemann, Andrea G. Allegrini, Elizabeth C. Corfield, Helga Ask, Neil M. Davies, Ole A. Andreassen, Alexandra Havdahl, & Laurie J. Hannigan (laurie.hannigan@fhi.no).
00_create_polygenic_scores.R creates polygenic scores (PGS) for mothers, fathers, and children, which are merged with the phenotypic data in the script ‘01_data_preparation.R’.

00.1_calculate_Neff_sumstats.R calculates the sample size (N) as the sum of effective sample sizes for each of the 11 GWAS meta-analyses.

00.2_create_pgs_job_scripts.R prepares the inputs for creating the 11 PGS via LDpred2.

01_data_preparation.R curates and selects the analytic dataset based on questionnaire, covariate, genotype, and registry data, including data manipulation and preparation of variables. It then creates an illustration of how differentiation is operationalized based on a subset of the raw data.

02_wave_specific_GWAS.R conducts wave-specific GWAS of the difference and total scores in Regenie at the ages of 1.5, 3, and 5 years. Scripts for conducting GWAS in Regenie are in the 'shell scripts/GWAS' folder. Additional preparation steps are conducted by the scripts 02.1-02.3.

02.1_prepare_sumstats.R prepares the summary statistics for multivariate GWAS in genomic SEM.

02.2_munge_sumstats.R munges the summary statistics before running ld-score regression.

02.3_ldsc.R runs LDSC on the summary statistics for running multivariate GWAS.

02.4_run_multivariate_GWAS_diff.R runs multivariate GWAS of the differentiation intercept from an LGM that is equivalent to the phenotypic LGM, but specified based on a genetic variance/covariance matrix. To be run on the cluster using the script '02.4_run_multivariate_GWAS_diff.sh' in the 'shell scripts/multivariate GWAS' folder.

02.5_run_multivariate_GWAS_tot.R runs multivariate GWAS of the total problem intercept from the same model as in 02.4, with differentiation and total problems constrained to be orthogonal (matching their relationship at the phenotypic level). To be run on the cluster using the script '02.5_run_multivariate_GWAS_tot.sh' in the 'shell scripts/multivariate GWAS' folder.

02.6_predicted_sample_size_GWAS.R checks the output from the multivariate GWAS, and calculates the predicted sample size for the latent intercept factors.

02.7_plot_results_GWAS.R creates miami plots, manhattan plots, and QQ plots based on the multivariate GWAS output.

03_genetic_correlations.R points to how genetic correlations of differentiation and total problems with the 11 psychiatric and neurodevelopmental conditions are estimated outside of R, in LDSC, with the scripts located in the folder 'shell scripts/genetic correlations'.

03.1_munge_multivar_sumstats.R munges the summary statistics from the multivariate GWAS of the differentiation and total problem intercepts. The munged summary statistics are then used to calculate genetic correlations in LDSC. To be ran on the cluster using the ‘03.1_munge_multivar_sumstats.sh' script located in the 'shell scripts/genetic correlations' folder.

03.2_munge_sumstats_wpsych.R munges the summary statistics for the 11 psychiatric and neurodevelopmental conditions before running LD-score regression. To be ran on the cluster using the ‘03.2_munge_sumstats_wpsych.sh' script located in the 'shell scripts/genetic correlations' folder.

03.3_run_ldsc_wpsych.R runs ld-score regression of differentiation and total problems, together with the 11 psychiatric conditions. To be ran on the cluster, using the '03.3_run_ldsc_wpsych.sh' script in the 'shell scripts/genetic correlations' folder.

03.4_plot_genetic_correlations.R plots genetic correlations of differentiation and total problems with the 11 psychiatric and neurodevelopmental conditions.

04_run_structural_models.R runs the correlated factor, hierarchical, and bifactor models with differentiation and total problems predicting the latent factors, by sourcing the script '04.1_specify_structural_models.R'. 

04.1_specify_structural_models.R specifies basic LGMs and three (previously established) structural models accounting for variance across 11 psychiatric and neurodevelopmental conditions, including differentiation and total problems as predictors. 

04.2_plot_structural_models.R creates plots of the structural models of liability to the 11 psychiatric and neurodevelopmental conditions, and resulting path estimates.

05_run_combined_LGMs_with_pgs.R runs the LGMs with PGS included as predictors of differentiation and total problems by sourcing the script ‘05.1_specify_combined_LGMs_with_pgs.R’. The models are: 1) basic latent growth model (LGM); 2) LGM with PGS as predictors (intercept and slope); 3) effects on intercepts only; and 4) effects on slopes only.

05.1_specify_combined_LGMs_with_pgs.R specifies a basic LGM and three versions of the LGM including child PGS as predictors and differentiation and total problems as outcomes.

06_run_trio_pgs_analyses.R runs LGMs with trio PGS as predictors by sourcing the script '06.1_specify_LGMs_with_trio_pgs.R'. The models are: 1) multivariate PGS model in the full sample of trios; 2) multivariate PGS model restricted to unrelated trios; and 3) univariate PGS models in the full sample of trios. Results are processed, plotted, and tabulated for each of the models.

06.1_specify_LGMs_with_trio_pgs.R specifies LGMs with trio PGS as predictors.

07_run_equivalence_tests.R runs equivalence tests to determine, in conjunction with null hypothesis significance tests, whether the PGS effects are null, non-null, or undecided.