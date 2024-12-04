#02.7_plot_results_GWAS.R

#This script creates miami plots, manhattan plots, and QQ plots based 
#on the multivariate GWAS output for differentiation and total problems.

#devtools::install_local("./packages/miamiplot-master.zip", lib = "./packages")

#load required packages
library(miamiplot, lib = "./packages")
library(qqman)
library(dplyr)
library(data.table)
library(ggplot2)

#load data for miami plot differentiation and total (diff top and tot bottom)
intercept_diff <- fread("N:/durable/projects/differentiation_genomics/GWAS/output_files/lgm/intercept_diff_new.txt")
intercept_tot <- fread("N:/durable/projects/differentiation_genomics/GWAS/output_files/lgm/intercept_tot_new.txt")

#merge diff and tot
both <- intercept_diff %>%
  full_join(intercept_tot)

rm(intercept_diff,intercept_tot)
gc()

#plot
miami_both<- ggmiami(data = both, 
                     split_by = "lhs", 
                     split_at = "i1", 
                     p = "P", chr = "CHR", pos = "BP",
                     genome_line = 5e-8,
                     genome_line_color = "blue",
                     suggestive_line = 1e-5,
                     suggestive_line_color = "blue",
                     chr_colors = NULL,
                     upper_chr_colors = c("#99CCFF","#0072B2"),
                     lower_chr_colors = c("#FF9933","#D55E00"),
                     upper_ylab = "Differentiation",
                     lower_ylab = "Total problems")

#save
ggsave(miami_both, filename = "Miami_both_big_new.tiff", device = "tiff", width = 9, 
       height = 4.5, units = "in", dpi = 1200)

##make Manhattan and QQ plots for the GWAS of intercepts
setwd("N:/durable/projects/differentiation_genomics/GWAS/output_files/lgm/")

Sys.getenv(c("DISPLAY"))
options(bitmapType='cairo') 

#list files differentiation
files <- list.files(path = "N:/durable/projects/differentiation_genomics/GWAS/output_files/lgm", pattern = "_diff.txt")

#create function for plotting
GWAS_plots_diff <- function(file){
  name <- stringr::str_remove(file, ".txt")
  
  # manhattan plot
  results_lin <- fread(file, head=TRUE)
  jpeg(paste0("Linear_manhattan_",name,".jpeg"), 
       width = 1024, height = 768, pointsize = 14, quality = 100)
  manhattan(results_lin, chr="CHR",bp="BP",p="P",snp="SNP", main = paste0("Manhattan plot: ", name), ylim = c(0,10),  cex = 0.6, cex.axis = 0.9, col = c("navy", "royalblue"), suggestiveline = -log10(1e-05), genomewideline = -log10(5e-08), chrlabs = c(1:22))
  dev.off()
  
  # QQ plot:
  jpeg(paste0("QQPlot_", name, ".jpeg") , 
       width = 600, height = 400, pointsize = 20, quality = 100)
  qq(results_lin$P)
  dev.off()
  
}

#plot
purrr::map(files, GWAS_plots_diff)

#list files for total
files <- list.files(path = "N:/durable/projects/differentiation_genomics/GWAS/output_files/lgm", pattern = "_tot.txt")

#create function for plotting
GWAS_plots_tot <- function(file){
  name <- stringr::str_remove(file, ".txt")
  
  # manhattan plot
  results_lin <- fread(file, head=TRUE)
  jpeg(paste0("Linear_manhattan_",name,".jpeg"), 
       width = 1024, height = 768, pointsize = 14, quality = 100)
  manhattan(results_lin, chr="CHR",bp="BP",p="P",snp="SNP", main = paste0("Manhattan plot: ", name), ylim = c(0,10),  cex = 0.6, cex.axis = 0.9, col = c("navy", "royalblue"), suggestiveline = -log10(1e-05), genomewideline = -log10(5e-08), chrlabs = c(1:22))
  dev.off()
  
  # QQ plot:
  jpeg(paste0("QQPlot_", name, ".jpeg") , 
       width = 600, height = 400, pointsize = 20, quality = 100)
  qq(results_lin$P)
  dev.off()
  
}

#plot
purrr::map(files, GWAS_plots_tot)

