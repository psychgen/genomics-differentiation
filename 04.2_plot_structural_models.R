#04.2_plot_structural_models.R

#This script creates plots of the structural models of genetic liability to 11 
#psychiatric and neurodevelopmental conditions, and resulting path estimates. 

#load necessary packages
library(tidyverse)
library(patchwork)
library(DiagrammeR)

## plot correlated factors model

#load results
load(file="./output/tmpDT4factors.RData")

#convert path data-frame to node and edge data-frame
ModelDT_4factors$results %>% as.data.frame %>% sample_n(5)

#obtain std estimates
ModelDT_4factors$results %>%
  mutate(est = round(STD_All, 2))

#to describe edges, we need the origin, path-type, endpoint, and label.
paths <- ModelDT_4factors$results %>%
  select(lhs, op, rhs, est=STD_All) %>%
  filter(rhs != "i1" & rhs != "s1" & rhs != "diff1" &
           rhs != "diff2" & rhs != "diff3" &
           rhs != "i2" & rhs != "s2" & rhs != "tot1" &
           rhs != "tot2" & rhs != "tot3")

#latent variables are left-hand side of "=~" lines
latent <- paths %>%
  filter(op == "=~") %>%
  select(nodes = lhs) %>%
  distinct %>%
  mutate(shape = "circle")

#this is a node data-frame now
latent

#manifest variables are not latent variables
`%not_in%` <- Negate(`%in%`)
manifest <- paths %>%
  filter(op != "~1", lhs %not_in% latent$nodes) %>%
  select(nodes = lhs) %>%
  distinct %>%
  mutate(shape = "square")

#nodes are prepared
node_set <- combine_nodes(latent, manifest)
node_set

#edges will be labeled by the parameter estimates
all_paths <- paths %>%
  filter(op != "~1") %>%
  mutate(label = round(est, 2)) %>%
  select(-est)

#factor loadings are the paths in the "=~" lines
loadings <- all_paths %>%
  filter(op == "=~") %>%
  mutate(edge_from = lhs, edge_to = rhs, style = "dashed") %>%
  select(edge_from, edge_to, style, label)

#this is now an edge dataframe
loadings

#regressions are the paths in the "~" lines
regressions <- all_paths %>%
  filter(op == "~") %>%
  rename(edge_to = lhs, edge_from = rhs) %>%
  mutate(style = "solid") %>%
  select(edge_from, edge_to, style, label)

edge_set <- combine_edges(loadings, regressions)
edge_set

#combine edges and nodes
my_graph <- graphviz_graph(
  nodes = node_set,
  edges_df = edge_set,
  graph_attrs = c("ranksep = 1"))

#plot the graph
graphviz_render(my_graph)

#extract dot code from this diagram
cat(my_graph$dot_code)

#tweak
grViz("
 
 digraph dot {
     
     graph [ranksep = 1]
     rankdir = 'BT'
     splines = false

     'com' [shape = 'circle', fixedsize = TRUE, label = 'com@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'psy' [shape = 'circle', fixedsize = TRUE, label = 'psy@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'neu' [shape = 'circle', fixedsize = TRUE, label = 'neu@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'int' [shape = 'circle', fixedsize = TRUE, label = 'int@_{g}', style = 'filled', fillcolor = 'ivory']  
     'an' [shape = 'square', fixedsize = TRUE, label = 'an@_{g}'] 
     'ocd' [shape = 'square', fixedsize = TRUE, label = 'ocd@_{g}'] 
     'ts' [shape = 'square', fixedsize = TRUE, label = 'ts@_{g}'] 
     'scz' [shape = 'square', fixedsize = TRUE, label = 'scz@_{g}'] 
     'bip' [shape = 'square', fixedsize = TRUE, label = 'bip@_{g}'] 
     'alc' [shape = 'square', fixedsize = TRUE, label = 'alc@_{g}'] 
     'adhd' [shape = 'square', fixedsize = TRUE, label = 'adhd@_{g}'] 
     'asd' [shape = 'square', fixedsize = TRUE, label = 'asd@_{g}'] 
     'ptsd' [shape = 'square', fixedsize = TRUE, label = 'ptsd@_{g}'] 
     'mdd' [shape = 'square', fixedsize = TRUE, label = 'mdd@_{g}'] 
     'anx' [shape = 'square', fixedsize = TRUE, label = 'anx@_{g}'] 
     'com'->'an' [style = 'solid', label = '.63  '] 
     'com'->'ocd' [style = 'solid', label = '  .76'] 
     'com'->'ts' [style = 'solid', label = ' .22'] 
     'psy'->'scz' [style = 'solid', label = '.78  '] 
     'psy'->'bip' [style = 'solid', headlabel = '  .87', labeldistance = 2, labelangle = 25] 
     'psy'->'alc' [style = 'solid', label = '.12'] 
     'neu'->'adhd' [style = 'solid', label = '    .69'] 
     'neu'->'asd' [style = 'solid', headlabel = '    .49 ', labeldistance = 1.8, labelangle = 20] 
     'neu'->'ptsd' [style = 'solid', label = '.20     '] 
     'neu'->'alc' [style = 'solid', label = '.40'] 
     'neu'->'mdd' [style = 'solid', headlabel = '.14 ', labeldistance = 7.8, labelangle = 8] 
     'neu'->'ts' [style = 'solid', headlabel = ' .18', labeldistance = 4.8, labelangle = -15] 
     'int'->'ptsd' [style = 'solid', label = ' .31'] 
     'int'->'mdd' [style = 'solid', headlabel = '.82 ', labeldistance = 4.5, labelangle = 15] 
     'int'->'anx' [style = 'solid', label = '.98 '] 
     'int'->'alc' [style = 'solid', headlabel = '   .19', labeldistance = 3.7, labelangle = -20] 
     
     #additional constraints on the graph
     
     an -> ocd -> ts -> scz -> bip -> alc -> adhd -> asd -> ptsd -> mdd -> anx [style = 'invis']
     com -> psy -> neu -> int [style = 'invis']
     {rank = 'max'; com; psy; neu; int}
     {rank = 'min'; an; ocd; ts; scz; bip; alc; adhd; asd; ptsd; mdd; anx}

 }
 ")

## plot hierarchical structural model

#load results
load(file="./output/both_hi_p.RData")

#convert path data-frame to node and edge data-frame
ModelDT_hi_p$results %>% as.data.frame %>% sample_n(5)

#to describe edges, we need the origin, path-type, endpoint, and label.
paths <- ModelDT_hi_p$results %>%
  select(lhs, op, rhs, est=STD_All)

#latent variables are left-hand side of "=~" lines
latent <- paths %>%
  filter(op == "=~") %>%
  select(nodes = lhs) %>%
  distinct %>%
  mutate(shape = "circle")

#this is a node data-frame now
latent

#manifest variables are not latent variables
`%not_in%` <- Negate(`%in%`)
manifest <- paths %>%
  filter(op != "~1", lhs %not_in% latent$nodes) %>%
  select(nodes = lhs) %>%
  distinct %>%
  mutate(shape = "square")

#nodes are prepared
node_set <- combine_nodes(latent, manifest)
node_set

#edges will be labeled by the parameter estimates
all_paths <- paths %>%
  filter(op != "~1") %>%
  mutate(label = round(est, 2)) %>%
  select(-est)

#factor loadings are the paths in the "=~" lines
loadings <- all_paths %>%
  filter(op == "=~") %>%
  mutate(edge_from = lhs, edge_to = rhs, style = "dashed") %>%
  select(edge_from, edge_to, style, label)

#this is now an edge dataframe
loadings

#regressions are the paths in the "~" lines
regressions <- all_paths %>%
  filter(op == "~") %>%
  rename(edge_to = lhs, edge_from = rhs) %>%
  mutate(style = "solid") %>%
  select(edge_from, edge_to, style, label)

edge_set <- combine_edges(loadings, regressions)
edge_set

#combine edges and nodes
my_graph <- graphviz_graph(
  nodes = node_set,
  edges_df = edge_set,
  graph_attrs = c("ranksep = 1"))

#plot the graph
graphviz_render(my_graph)

#extract dot code from this diagram
cat(my_graph$dot_code)

#tweak
grViz("
 
 digraph dot {
     
     graph [ranksep = 1]
     rankdir = 'BT'
     splines = false
     
     'p' [shape = 'circle', fixedsize = TRUE, label = 'p@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'com' [shape = 'circle', fixedsize = TRUE, label = 'com@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'psy' [shape = 'circle', fixedsize = TRUE, label = 'psy@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'neu' [shape = 'circle', fixedsize = TRUE, label = 'neu@_{g}', style = 'filled', fillcolor = 'ivory'] 
     'int' [shape = 'circle', fixedsize = TRUE, label = 'int@_{g}', style = 'filled', fillcolor = 'ivory']  
     'an' [shape = 'square', fixedsize = TRUE, label = 'an@_{g}'] 
     'ocd' [shape = 'square', fixedsize = TRUE, label = 'ocd@_{g}'] 
     'ts' [shape = 'square', fixedsize = TRUE, label = 'ts@_{g}'] 
     'scz' [shape = 'square', fixedsize = TRUE, label = 'scz@_{g}'] 
     'bip' [shape = 'square', fixedsize = TRUE, label = 'bip@_{g}'] 
     'alc' [shape = 'square', fixedsize = TRUE, label = 'alc@_{g}'] 
     'adhd' [shape = 'square', fixedsize = TRUE, label = 'adhd@_{g}'] 
     'asd' [shape = 'square', fixedsize = TRUE, label = 'asd@_{g}'] 
     'ptsd' [shape = 'square', fixedsize = TRUE, label = 'ptsd@_{g}'] 
     'mdd' [shape = 'square', fixedsize = TRUE, label = 'mdd@_{g}'] 
     'anx' [shape = 'square', fixedsize = TRUE, label = 'anx@_{g}'] 
     'com'->'an' [style = 'solid', label = '.63  '] 
     'com'->'ocd' [style = 'solid', label = '  .69'] 
     'com'->'ts' [style = 'solid', label = ' .27'] 
     'psy'->'scz' [style = 'solid', label = '.79  '] 
     'psy'->'bip' [style = 'solid', label = '    .86'] 
     'psy'->'alc' [style = 'solid', label = '.13'] 
     'neu'->'adhd' [style = 'solid', label = ' .65'] 
     'neu'->'asd' [style = 'solid', headlabel = '    .56 ', labeldistance = 2, labelangle = 20] 
     'neu'->'ptsd' [style = 'solid', label = '.47      '] 
     'neu'->'alc' [style = 'solid', label = '0   '] 
     'neu'->'mdd' [style = 'solid', headlabel = '.14 ', labeldistance = 8.4, labelangle = 8] 
     'neu'->'ts' [style = 'solid', headlabel = ' .14', labeldistance = 4.7, labelangle = -15] 
     'int'->'ptsd' [style = 'solid', label = ' .10'] 
     'int'->'mdd' [style = 'solid', headlabel = '.83 ', labeldistance = 4.5, labelangle = 15] 
     'int'->'anx' [style = 'solid', label = '.98 '] 
     'int'->'alc' [style = 'solid', headlabel = '   .48', labeldistance = 3.6, labelangle = -20] 
     'p'->'com' [style = 'solid', label = '.53'] 
     'p'->'psy' [style = 'solid', label = '.56'] 
     'p'->'neu' [style = 'solid', label = '.70'] 
     'p'->'int' [style = 'solid', label = '.87'] 
     
     #additional constraints on the graph
     
     an -> ocd -> ts -> scz -> bip -> alc -> adhd -> asd -> ptsd -> mdd -> anx [style = 'invis']
     com -> psy -> neu -> int [style = 'invis']
     {rank = 'max'; p}
     {rank = 'same'; com; psy; neu; int}
     {rank = 'min'; an; ocd; ts; scz; bip; alc; adhd; asd; ptsd; mdd; anx}

 }
 ")

## plot path estimates from differentiation and total problems

#load results
load(file="./output/tmpDT4factors.RData")

#differentiation - correlated factors
estsD_cor_all <- tmpDT4factors %>% 
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  filter(rhs=="i1"&op!="~~") %>%
  mutate(fdr_pval = p.adjust(p_value, method = "fdr")) %>%
  select(lhs,model,STD_All,fdr_pval) %>%
  `colnames<-`(c("outcome","model","STD_All","fdr_pval")) %>% 
  mutate(est = (STD_All^2),
         model = "diff_4factors")

# total problems - correlated factors
estsT_cor_all <- tmpDT4factors %>% 
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  filter(rhs=="i2"&op!="~~") %>%
  mutate(fdr_pval = p.adjust(p_value, method = "fdr")) %>%
  select(lhs,model,STD_All,fdr_pval) %>%
  `colnames<-`(c("outcome","model","STD_All","fdr_pval")) %>% 
  mutate(est = (STD_All^2),
         model = "tot_4factors")

# merge diff and tot
ests_cor_both <- estsD_cor_all %>%
  select(-STD_All,-fdr_pval) %>% 
  full_join(estsT_cor_all %>%
              select(-STD_All,-fdr_pval)) %>%
  pivot_wider(names_from = model, values_from = est) %>% 
  mutate(diff_propwfact = diff_4factors/(diff_4factors+tot_4factors),
         tot_propwfact = tot_4factors/(diff_4factors+tot_4factors)) %>%
  pivot_longer(cols = -c(outcome), 
             names_to = c("model", ".value"),
             names_sep = "_") %>%
  mutate(model = as.factor(model),
         denom = sum(`4factors`),
         prop = `4factors`/denom) %>%
  select(model,outcome,prop,propwfact,est=`4factors`)

# rename factor levels
levels(ests_cor_both$model)[levels(ests_cor_both$model)=="diff"] <- "Differentiation"
levels(ests_cor_both$model)[levels(ests_cor_both$model)=="tot"] <- "Total problems"

## neurodevelopmental factor plot correlated factors

# restrict to neu and create % labels
ests_cor_both_neu <- ests_cor_both %>%
  filter(outcome=="neu") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_neu$ymax <- cumsum(ests_cor_both_neu$propwfact)

# compute the bottom of rectangle
ests_cor_both_neu$ymin <- c(0, head(ests_cor_both_neu$ymax, n=-1))

# compute the size of rectangle
ests_cor_both_neu$xmax <- 1.25
ests_cor_both_neu$xmin <- ests_cor_both_neu$xmax-0.45

# compute label positions on both axes
ests_cor_both_neu$labelPosition.y <- (ests_cor_both_neu$ymax + ests_cor_both_neu$ymin) / 2
ests_cor_both_neu$labelPosition.x <- ests_cor_both_neu$xmax + 0.8

# create donut chart correlated factor model (neurodevelopmental factor)
neu1<-ggplot(ests_cor_both_neu, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void()

neu1

## internalising factor plot correlated factors

# restrict to int and create % labels
ests_cor_both_int <- ests_cor_both %>%
  filter(outcome=="int") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_int$ymax <- cumsum(ests_cor_both_int$propwfact)

# compute the bottom of each rectangle
ests_cor_both_int$ymin <- c(0, head(ests_cor_both_int$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_int$xmax <- 1.25
ests_cor_both_int$xmin <- ests_cor_both_int$xmax-0.45

# compute label positions on both axes
ests_cor_both_int$labelPosition.y <- (ests_cor_both_int$ymax + ests_cor_both_int$ymin) / 2
ests_cor_both_int$labelPosition.x <- ests_cor_both_int$xmax + 0.6


# create donut chart correlated factor model (int factor)
int1<-ggplot(ests_cor_both_int, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void()

int1


## compulsive factor plot correlated factors
# restrict to com and create % labels
ests_cor_both_com <- ests_cor_both %>%
  filter(outcome=="com") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_com$ymax <- cumsum(ests_cor_both_com$propwfact)

# compute the bottom of each rectangle
ests_cor_both_com$ymin <- c(0, head(ests_cor_both_com$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_com$xmax <- 1.25
ests_cor_both_com$xmin <- ests_cor_both_com$xmax-0.45

# compute label positions on both axes
ests_cor_both_com$labelPosition.y <- (ests_cor_both_com$ymax + ests_cor_both_com$ymin) / 2
ests_cor_both_com$labelPosition.x <- ests_cor_both_com$xmax + 0.7


# create donut chart correlated factor model (com factor)
com1<-ggplot(ests_cor_both_com, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B245","#D55E0045")) +
  scale_fill_manual(values= c("#0072B245","#D55E0045")) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void()

com1


## psychotic factor plot correlated factors
# restrict to psy and create % labels
ests_cor_both_psy <- ests_cor_both %>%
  filter(outcome=="psy") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_psy$ymax <- cumsum(ests_cor_both_psy$propwfact)

# compute the bottom of each rectangle
ests_cor_both_psy$ymin <- c(0, head(ests_cor_both_psy$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_psy$xmax <- 1.25
ests_cor_both_psy$xmin <- ests_cor_both_psy$xmax-0.45

# compute label positions on both axes
ests_cor_both_psy$labelPosition.y <- (ests_cor_both_psy$ymax + ests_cor_both_psy$ymin) / 2
ests_cor_both_psy$labelPosition.x <- ests_cor_both_psy$xmax + 0.75


# create donut chart correlated factor model (psy factor)
psy1<-ggplot(ests_cor_both_psy, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B245","#D55E0045")) +
  scale_fill_manual(values= c("#0072B245","#D55E0045")) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void()

psy1

patch1 <- (com1 + theme(legend.position = "none")) |
  (psy1 + theme(legend.position = "none")) |
  (neu1 + theme(legend.position = "top", 
                legend.title = element_blank(),
                legend.text = element_text(size=15),
                legend.box.margin = margin(0, 220, 0, 0))) | 
  (int1 + theme(legend.position = "none"))

patch1


## plot hierarchical model path estimates (differentiation and total problems
## predicting p and sub-factors)

# load results
load(file="./output/both_hi_p.RData")
load(file="./output/both_hi_sub.RData")

# use hierarchical model to compare diff effects with total for the p-factor

# differentiation ~ hierarchical
estsD_hi_all <- both_hi_p %>% 
  full_join(both_hi_sub) %>%
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  filter(rhs=="i1"&op!="~~") %>%
  mutate(fdr_pval = p.adjust(p_value, method = "fdr")) %>%
  select(lhs,model,STD_All,fdr_pval) %>%
  `colnames<-`(c("outcome","model","STD_All","fdr_pval")) %>% 
  mutate(est = STD_All^2,
         model = "diff_hier_all")

# total problems ~ hierarchical
estsT_hi_all <- both_hi_p %>% 
  full_join(both_hi_sub) %>%
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  filter(rhs=="i2"&op!="~~") %>%
  mutate(fdr_pval = p.adjust(p_value, method = "fdr")) %>%
  select(lhs,model,STD_All,fdr_pval) %>%
  `colnames<-`(c("outcome","model","STD_All","fdr_pval")) %>% 
  mutate(est = STD_All^2,
         model = "tot_hier_all")

# merge diff and tot (p-factor)
ests_hi_both_p <- estsD_hi_all %>%
  select(-STD_All,-fdr_pval) %>% 
  full_join(estsT_hi_all %>%
              select(-STD_All,-fdr_pval)) %>%
  filter(outcome=="p") %>% 
  mutate(model=str_remove(model,"_all")) %>%
  pivot_wider(names_from = model, values_from = est) %>% 
  mutate(diff_propwfact = diff_hier/(diff_hier+tot_hier),
         tot_propwfact = tot_hier/(diff_hier+tot_hier)) %>%
  pivot_longer(cols = -c(outcome), 
               names_to = c("model", ".value"),
               names_sep = "_") %>%
  mutate(model = as.factor(model),
         denom = sum(`hier`),
         prop = `hier`/denom) %>%
  select(model,outcome,prop,propwfact,est=`hier`)

ests_hi_both_sub <- estsD_hi_all %>%
  select(-STD_All,-fdr_pval) %>% 
  full_join(estsT_hi_all %>%
              select(-STD_All,-fdr_pval)) %>%
  filter(outcome!="p") %>% 
  mutate(model=str_remove(model,"_all")) %>%
  pivot_wider(names_from = model, values_from = est) %>% 
  mutate(diff_propwfact = diff_hier/(diff_hier+tot_hier),
         tot_propwfact = tot_hier/(diff_hier+tot_hier)) %>%
  pivot_longer(cols = -c(outcome), 
               names_to = c("model", ".value"),
               names_sep = "_") %>%
  mutate(model = as.factor(model),
         denom = sum(`hier`),
         prop = `hier`/denom) %>%
  select(model,outcome,prop,propwfact,est=`hier`)

# rename factor levels
levels(ests_hi_both_p$model)[levels(ests_hi_both_p$model)=="diff"] <- "Differentiation"
levels(ests_hi_both_p$model)[levels(ests_hi_both_p$model)=="tot"] <- "Total problems"
levels(ests_hi_both_sub$model)[levels(ests_hi_both_sub$model)=="diff"] <- "Differentiation"
levels(ests_hi_both_sub$model)[levels(ests_hi_both_sub$model)=="tot"] <- "Total problems"

## p factor plot

ests_hi_both_p <- ests_hi_both_p %>%
  filter(outcome=="p") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_p$ymax <- cumsum(ests_hi_both_p$propwfact)

# compute the bottom of each rectangle
ests_hi_both_p$ymin <- c(0, head(ests_hi_both_p$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_p$xmax <- 1.25
ests_hi_both_p$xmin <- ests_hi_both_p$xmax-0.45

# compute label positions on both axes
ests_hi_both_p$labelPosition.y <- (ests_hi_both_p$ymax + ests_hi_both_p$ymin) / 2
ests_hi_both_p$labelPosition.x <- ests_hi_both_p$xmax + 0.8

p<-ggplot(ests_hi_both_p, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.spacing = unit(2, "cm"))

p

## neurodevelopmental factor plot

ests_hi_both_neu <- ests_hi_both_sub %>%
  filter(outcome=="neu") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_neu$ymax <- cumsum(ests_hi_both_neu$propwfact)

# compute the bottom of each rectangle
ests_hi_both_neu$ymin <- c(0, head(ests_hi_both_neu$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_neu$xmax <- 1.25
ests_hi_both_neu$xmin <- ests_hi_both_neu$xmax-0.45

# compute label positions on both axes
ests_hi_both_neu$labelPosition.y <- (ests_hi_both_neu$ymax + ests_hi_both_neu$ymin) / 2
ests_hi_both_neu$labelPosition.x <- ests_hi_both_neu$xmax + 1.2

neu<-ggplot(ests_hi_both_neu, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x,label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

neu

## internalising factor plot

ests_hi_both_int <- ests_hi_both_sub %>%
  filter(outcome=="int") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_int$ymax <- cumsum(ests_hi_both_int$propwfact)

# compute the bottom of each rectangle
ests_hi_both_int$ymin <- c(0, head(ests_hi_both_int$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_int$xmax <- 1.25
ests_hi_both_int$xmin <- ests_hi_both_int$xmax-0.45

# compute label positions on both axes
ests_hi_both_int$labelPosition.y <- (ests_hi_both_int$ymax + ests_hi_both_int$ymin) / 2
ests_hi_both_int$labelPosition.x <- ests_hi_both_int$xmax + 0.7

int<-ggplot(ests_hi_both_int, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

int

## compulsive factor plot

ests_hi_both_com <- ests_hi_both_sub %>%
  filter(outcome=="com") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_com$ymax <- cumsum(ests_hi_both_com$propwfact)

# compute the bottom of each rectangle
ests_hi_both_com$ymin <- c(0, head(ests_hi_both_com$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_com$xmax <- 1.25
ests_hi_both_com$xmin <- ests_hi_both_com$xmax-0.45

# compute label positions on both axes
ests_hi_both_com$labelPosition.y <- (ests_hi_both_com$ymax + ests_hi_both_com$ymin) / 2
ests_hi_both_com$labelPosition.x <- ests_hi_both_com$xmax + 1

com<-ggplot(ests_hi_both_com, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B245","#D55E0045")) +
  scale_fill_manual(values= c("#0072B245","#D55E0045")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

com

## psychotic factor plot

ests_hi_both_psy <- ests_hi_both_sub %>%
  filter(outcome=="psy") %>%
  mutate(perc_labels = scales::percent(propwfact,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_psy$ymax <- cumsum(ests_hi_both_psy$propwfact)

# compute the bottom of each rectangle
ests_hi_both_psy$ymin <- c(0, head(ests_hi_both_psy$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_psy$xmax <- 1.25
ests_hi_both_psy$xmin <- ests_hi_both_psy$xmax-0.45

# compute label positions on both axes
ests_hi_both_psy$labelPosition.y <- (ests_hi_both_psy$ymax + ests_hi_both_psy$ymin) / 2
ests_hi_both_psy$labelPosition.x <- ests_hi_both_psy$xmax + 1.1

psy<-ggplot(ests_hi_both_psy, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  scale_colour_manual(values= c("#0072B245","#D55E0045")) +
  scale_fill_manual(values= c("#0072B245","#D55E0045")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

psy

patch <- (plot_spacer() | plot_spacer() | p | guide_area() + plot_layout(guides="collect")) / (com | psy | neu | int)

patch
