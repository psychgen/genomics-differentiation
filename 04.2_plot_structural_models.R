#04.2_plot_structural_models.R

## This script creates plots of the structural models of liability
## to 11 psychiatric conditions, and resulting path estimates. 

#load necessary packages
library(tidyverse)
library(patchwork)
library(DiagrammeR)

## plot correlated factors model

#load results
load(file="./output/tmpD4factors.RData")

#convert path data-frame to node and edge data-frame
ModelD_4factors$results %>% as.data.frame %>% sample_n(5)

#obtain 
ModelD_4factors$results %>%
  mutate(est = round(STD_All, 2))

#to describe edges, we need the origin, path-type, endpoint, and label.
paths <- ModelD_4factors$results %>%
  select(lhs, op, rhs, est=STD_All) %>%
  filter(lhs != "i" & lhs != "s" & lhs!= "diff1" &
           lhs != "diff2" & lhs != "diff3")

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
     'com'->'an' [style = 'solid', label = '.62  '] 
     'com'->'ocd' [style = 'solid', label = '  .76'] 
     'com'->'ts' [style = 'solid', label = ' .25'] 
     'psy'->'scz' [style = 'solid', label = '.78  '] 
     'psy'->'bip' [style = 'solid', headlabel = '  .87', labeldistance = 2, labelangle = 25] 
     'psy'->'alc' [style = 'solid', label = '.13'] 
     'neu'->'adhd' [style = 'solid', label = '    .69'] 
     'neu'->'asd' [style = 'solid', headlabel = '    .50 ', labeldistance = 1.8, labelangle = 20] 
     'neu'->'ptsd' [style = 'solid', label = '.31     '] 
     'neu'->'alc' [style = 'solid', label = '.38'] 
     'neu'->'mdd' [style = 'solid', headlabel = '.17 ', labeldistance = 7.8, labelangle = 8] 
     'neu'->'ts' [style = 'solid', headlabel = ' .14', labeldistance = 4.8, labelangle = -15] 
     'int'->'ptsd' [style = 'solid', label = ' .24'] 
     'int'->'mdd' [style = 'solid', headlabel = '.81 ', labeldistance = 4.5, labelangle = 15] 
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
load(file="./output/diff_hier_p.RData")

#convert path data-frame to node and edge data-frame
ModelD_hi_p$results %>% as.data.frame %>% sample_n(5)

#to describe edges, we need the origin, path-type, endpoint, and label.
paths <- ModelD_hi_p$results %>%
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
     'com'->'an' [style = 'solid', label = '.62  '] 
     'com'->'ocd' [style = 'solid', label = '  .72'] 
     'com'->'ts' [style = 'solid', label = ' .26'] 
     'psy'->'scz' [style = 'solid', label = '.78  '] 
     'psy'->'bip' [style = 'solid', label = '    .87'] 
     'psy'->'alc' [style = 'solid', label = '.12'] 
     'neu'->'adhd' [style = 'solid', label = ' .65'] 
     'neu'->'asd' [style = 'solid', headlabel = '    .58 ', labeldistance = 2, labelangle = 20] 
     'neu'->'ptsd' [style = 'solid', label = '.50      '] 
     'neu'->'alc' [style = 'solid', label = '.11   '] 
     'neu'->'mdd' [style = 'solid', headlabel = '.28 ', labeldistance = 8.4, labelangle = 8] 
     'neu'->'ts' [style = 'solid', headlabel = ' .14', labeldistance = 4.7, labelangle = -15] 
     'int'->'ptsd' [style = 'solid', label = ' .12'] 
     'int'->'mdd' [style = 'solid', headlabel = '.74 ', labeldistance = 4.5, labelangle = 15] 
     'int'->'anx' [style = 'solid', label = '.60 '] 
     'int'->'alc' [style = 'solid', headlabel = '   .41', labeldistance = 3.6, labelangle = -20] 
     'p'->'com' [style = 'solid', label = '.57'] 
     'p'->'psy' [style = 'solid', label = '.61'] 
     'p'->'neu' [style = 'solid', label = '.60'] 
     'p'->'int' [style = 'solid', label = '.80'] 
     
     #additional constraints on the graph
     
     an -> ocd -> ts -> scz -> bip -> alc -> adhd -> asd -> ptsd -> mdd -> anx [style = 'invis']
     com -> psy -> neu -> int [style = 'invis']
     {rank = 'max'; p}
     {rank = 'same'; com; psy; neu; int}
     {rank = 'min'; an; ocd; ts; scz; bip; alc; adhd; asd; ptsd; mdd; anx}

 }
 ")

#load results
load(file="./output/tmpD4factors.RData")
load(file="./output/tmpT4factors.RData")

#differentiation ~ correlated factors
estsD_cor_all <- tmpD4factors %>% 
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  select(op,rhs,model,STD_All) %>%
  pivot_wider(names_from = op, values_from = STD_All) %>%
  `colnames<-`(c("predictor","model","STD_All","var_diff")) %>% 
  mutate(est = (STD_All^2)*var_diff)

# total problems ~ correlated factors
estsT_cor_all <- tmpT4factors %>% 
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  select(op,rhs,model,STD_All) %>%
  pivot_wider(names_from = op, values_from = STD_All) %>%
  `colnames<-`(c("predictor","model","STD_All","var_tot")) %>% 
  mutate(est = (STD_All^2)*var_tot)

# merge diff and tot
ests_cor_both <- estsD_cor_all %>%
  select(-STD_All,-var_diff) %>% 
  full_join(estsT_cor_all %>%
              select(-STD_All,-var_tot)) %>%
  mutate(model=as.factor(model)) %>% 
  pivot_wider(names_from = model, values_from = est) %>% 
  mutate(diff_prop = diff_4factors/(diff_4factors+tot_4factors),
         tot_prop = tot_4factors/(diff_4factors+tot_4factors),
         overall = diff_4factors+tot_4factors,
         scaled_overall = log(overall,5 ) +3.1) %>%
  pivot_longer(cols = -c(predictor, overall, scaled_overall), 
             names_to = c("model", ".value"),
             names_sep = "_") %>%
  mutate(model = as.factor(model)) %>%
  select(model,predictor,overall,scaled_overall,prop,est=`4factors`)

# rename factor levels
levels(ests_cor_both$model)[levels(ests_cor_both$model)=="diff"] <- "Differentiation"
levels(ests_cor_both$model)[levels(ests_cor_both$model)=="tot"] <- "Total problems"

## neurodevelopmental factor plot correlated factors

# restrict to neu and create % labels
ests_cor_both_neu <- ests_cor_both %>%
  filter(predictor=="neu") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_neu$ymax <- cumsum(ests_cor_both_neu$prop)

# compute the bottom of each rectangle
ests_cor_both_neu$ymin <- c(0, head(ests_cor_both_neu$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_neu$xmax <- ests_cor_both_neu$scaled_overall
ests_cor_both_neu$xmin <- ests_cor_both_neu$xmax-0.45

# compute label positions on both axes
ests_cor_both_neu$labelPosition.y <- (ests_cor_both_neu$ymax + ests_cor_both_neu$ymin) / 2
ests_cor_both_neu$labelPosition.x <- ests_cor_both_neu$xmax + 0.65

# r-squared
ests_cor_both_neu$overall_rounded <- round(ests_cor_both_neu$overall/2, 2)

# create the expressions for the labels
ests_cor_both_neu$label_expr <- paste0("R^2 == ", ests_cor_both_neu$overall_rounded)

# create donut chart correlated factor model (neurodevelopmental factor)
neu1<-ggplot(ests_cor_both_neu, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -1, y = 0, label = label_expr), size = 5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

neu1

## internalising factor plot correlated factors

# restrict to int and create % labels
ests_cor_both_int <- ests_cor_both %>%
  filter(predictor=="int") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_int$ymax <- cumsum(ests_cor_both_int$prop)

# compute the bottom of each rectangle
ests_cor_both_int$ymin <- c(0, head(ests_cor_both_int$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_int$xmax <- ests_cor_both_int$scaled_overall
ests_cor_both_int$xmin <- ests_cor_both_int$xmax-0.45

# compute label positions on both axes
ests_cor_both_int$labelPosition.y <- (ests_cor_both_int$ymax + ests_cor_both_int$ymin) / 2
ests_cor_both_int$labelPosition.x <- ests_cor_both_int$xmax + 0.65

# r-squared
ests_cor_both_int$overall_rounded <- round(ests_cor_both_int$overall/2, 2)

# create the expressions for the labels
ests_cor_both_int$label_expr <- paste0("R^2 == ", ests_cor_both_int$overall_rounded)

# create donut chart correlated factor model (int factor)
int1<-ggplot(ests_cor_both_int, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -0.89, y = 0, label = label_expr), size = 5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

int1

## compulsive factor plot correlated factors

# restrict to com and create % labels
ests_cor_both_com <- ests_cor_both %>%
  filter(predictor=="com") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_com$ymax <- cumsum(ests_cor_both_com$prop)

# compute the bottom of each rectangle
ests_cor_both_com$ymin <- c(0, head(ests_cor_both_com$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_com$xmax <- ests_cor_both_com$scaled_overall
ests_cor_both_com$xmin <- ests_cor_both_com$xmax-0.45

# compute label positions on both axes
ests_cor_both_com$labelPosition.y <- (ests_cor_both_com$ymax + ests_cor_both_com$ymin) / 2
ests_cor_both_com$labelPosition.x <- ests_cor_both_com$xmax + 0.65

# r-squared
ests_cor_both_com$overall_rounded <- round(ests_cor_both_com$overall/2, 2)

# create the expressions for the labels
ests_cor_both_com$label_expr <- paste0("R^2 == ", ests_cor_both_com$overall_rounded)

# create donut chart correlated factor model (com factor)
com1<-ggplot(ests_cor_both_com, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -0.89, y = 0, label = label_expr), size = 5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

com1

## psychotic factor plot correlated factors

# restrict to psy and create % labels
ests_cor_both_psy <- ests_cor_both %>%
  filter(predictor=="psy") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_cor_both_psy$ymax <- cumsum(ests_cor_both_psy$prop)

# compute the bottom of each rectangle
ests_cor_both_psy$ymin <- c(0, head(ests_cor_both_psy$ymax, n=-1))

# compute the size of each rectangle
ests_cor_both_psy$xmax <- ests_cor_both_psy$scaled_overall
ests_cor_both_psy$xmin <- ests_cor_both_psy$xmax-0.45

# compute label positions on both axes
ests_cor_both_psy$labelPosition.y <- (ests_cor_both_psy$ymax + ests_cor_both_psy$ymin) / 2
ests_cor_both_psy$labelPosition.x <- ests_cor_both_psy$xmax + 0.5

# r-squared
ests_cor_both_psy$overall_rounded <- round(ests_cor_both_psy$overall/2, 2)

# create the expressions for the labels
ests_cor_both_psy$label_expr <- paste0("R^2 == ", ests_cor_both_psy$overall_rounded)

# create donut chart correlated factor model (psy factor)
psy1<-ggplot(ests_cor_both_psy, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -0.89, y = 0, label = label_expr), size = 5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

psy1

patch1 <- (com1 | psy1 | neu1 | int1) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.text = element_text(size=17))

patch1


# hierarchical model results (p and sub-factors)

# load results
load(file="./output/diff_hier_p.RData")
load(file="./output/diff_hier_sub.RData")
load(file="./output/tot_hier_p.RData")
load(file="./output/tot_hier_sub.RData")

# merge hierarchical diff results
diff_hier_all <- diff_hier_p %>%
  full_join(diff_hier_sub)

# hierarchical tot model results (p and sub-factors)

# merge hierarchical tot results
tot_hier_all <- tot_hier_p %>%
  full_join(tot_hier_sub)

# use hierarchical model to compare diff effects with total for the p-factor,
# as no total bifactor models converged

#differentiation ~ hierarchical
estsD_hi_all <- diff_hier_all %>% 
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  select(op,rhs,model,STD_All) %>%
  pivot_wider(names_from = op, values_from = STD_All) %>%
  `colnames<-`(c("predictor","model","STD_All","var_diff")) %>% 
  mutate(est = (STD_All^2)*var_diff)

# total problems ~ hierarchical
estsT_hi_all <- tot_hier_all %>% 
  mutate(Unstand_SE = as.numeric(Unstand_SE),
         STD_Genotype_SE = as.numeric(STD_Genotype_SE),
         across(where(is.numeric), ~ round(., 3))) %>%
  select(op,rhs,model,STD_All) %>%
  pivot_wider(names_from = op, values_from = STD_All) %>%
  `colnames<-`(c("predictor","model","STD_All","var_tot")) %>% 
  mutate(est = (STD_All^2)*var_tot)

# merge diff and tot
ests_hi_both <- estsD_hi_all %>%
  select(-STD_All,-var_diff) %>% 
  full_join(estsT_hi_all %>%
              select(-STD_All,-var_tot)) %>%
  mutate(model=str_remove(model,"_sub"),
         model=str_remove(model,"_p")) %>% 
  pivot_wider(names_from = model, values_from = est) %>% 
  mutate(diff_prop = diff_hier/(diff_hier+tot_hier),
         tot_prop = tot_hier/(diff_hier+tot_hier),
         overall = diff_hier+tot_hier,
         scaled_overall = log(overall,8) + 3.3) %>%
  pivot_longer(cols = -c(predictor, overall, scaled_overall), 
               names_to = c("model", ".value"),
               names_sep = "_") %>%
  mutate(model = as.factor(model)) %>%
  select(model,predictor,overall,scaled_overall,prop,est=hier)

# rename factor levels
levels(ests_hi_both$model)[levels(ests_hi_both$model)=="diff"] <- "Differentiation"
levels(ests_hi_both$model)[levels(ests_hi_both$model)=="tot"] <- "Total problems"

# restrict to sub-factors
ests_hi_both_sub <- ests_hi_both %>%
  filter(predictor!="p") %>%
  mutate(model=as.factor(model))

# restrict to p factor
ests_hi_both_p <- ests_hi_both %>%
  filter(predictor=="p") %>%
  mutate(model=as.factor(model))

## p factor plot

# restrict to p and create % labels
ests_hi_both_p <- ests_hi_both_p %>%
  filter(predictor=="p") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_p$ymax <- cumsum(ests_hi_both_p$prop)

# compute the bottom of each rectangle
ests_hi_both_p$ymin <- c(0, head(ests_hi_both_p$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_p$xmax <- ests_hi_both_p$scaled_overall
ests_hi_both_p$xmin <- ests_hi_both_p$xmax-0.45

# compute label positions on both axes
ests_hi_both_p$labelPosition.y <- (ests_hi_both_p$ymax + ests_hi_both_p$ymin) / 2
ests_hi_both_p$labelPosition.x <- ests_hi_both_p$xmax + 0.95

# r-squared
ests_hi_both_p$overall_rounded <- round(ests_hi_both_p$overall/2, 2)

# create the expressions for the labels
ests_hi_both_p$label_expr <- paste0("R^2 == ", ests_hi_both_p$overall_rounded)

# create donut chart correlated factor model (p factor)
p<-ggplot(ests_hi_both_p, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -1, y = 0, label = label_expr), size = 4.5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "right",
        legend.text = element_text(size=19))

p

## neurodevelopmental factor plot

# restrict to neu and create % labels
ests_hi_both_neu <- ests_hi_both_sub %>%
  filter(predictor=="neu") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_neu$ymax <- cumsum(ests_hi_both_neu$prop)

# compute the bottom of each rectangle
ests_hi_both_neu$ymin <- c(0, head(ests_hi_both_neu$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_neu$xmax <- ests_hi_both_neu$scaled_overall
ests_hi_both_neu$xmin <- ests_hi_both_neu$xmax-0.43

# compute label positions on both axes
ests_hi_both_neu$labelPosition.y <- (ests_hi_both_neu$ymax + ests_hi_both_neu$ymin) / 2
ests_hi_both_neu$labelPosition.x <- ests_hi_both_neu$xmax + 0.66

# r-squared
ests_hi_both_neu$overall_rounded <- round(ests_hi_both_neu$overall/2, 2)

# create the expressions for the labels
ests_hi_both_neu$label_expr <- paste0("R^2 == ", ests_hi_both_neu$overall_rounded)

# create donut chart correlated factor model (neurodevelopmental factor)
neu<-ggplot(ests_hi_both_neu, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(y=c(0.43,0.92),aes(x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -1, y = 0, label = label_expr), size = 4.5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

neu

## internalising factor plot

# restrict to neu and create % labels
ests_hi_both_int <- ests_hi_both_sub %>%
  filter(predictor=="int") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_int$ymax <- cumsum(ests_hi_both_int$prop)

# compute the bottom of each rectangle
ests_hi_both_int$ymin <- c(0, head(ests_hi_both_int$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_int$xmax <- ests_hi_both_int$scaled_overall
ests_hi_both_int$xmin <- ests_hi_both_int$xmax-0.45

# compute label positions on both axes
ests_hi_both_int$labelPosition.y <- (ests_hi_both_int$ymax + ests_hi_both_int$ymin) / 2
ests_hi_both_int$labelPosition.x <- ests_hi_both_int$xmax + 0.8

# r-squared
ests_hi_both_int$overall_rounded <- round(ests_hi_both_int$overall/2, 2)

# create the expressions for the labels
ests_hi_both_int$label_expr <- paste0("R^2 == ", ests_hi_both_int$overall_rounded)

# create donut chart correlated factor model (internalising factor)
int<-ggplot(ests_hi_both_int, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -1, y = 0, label = label_expr), size = 4.5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

int

## compulsive factor plot

# restrict to neu and create % labels
ests_hi_both_com <- ests_hi_both_sub %>%
  filter(predictor=="com") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_com$ymax <- cumsum(ests_hi_both_com$prop)

# compute the bottom of each rectangle
ests_hi_both_com$ymin <- c(0, head(ests_hi_both_com$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_com$xmax <- ests_hi_both_com$scaled_overall
ests_hi_both_com$xmin <- ests_hi_both_com$xmax-0.45

# compute label positions on both axes
ests_hi_both_com$labelPosition.y <- (ests_hi_both_com$ymax + ests_hi_both_com$ymin) / 2
ests_hi_both_com$labelPosition.x <- ests_hi_both_com$xmax + 0.8

# r-squared
ests_hi_both_com$overall_rounded <- round(ests_hi_both_com$overall/2, 2)

# create the expressions for the labels
ests_hi_both_com$label_expr <- paste0("R^2 == ", ests_hi_both_com$overall_rounded)

# create donut chart correlated factor model (compulsive factor)
com<-ggplot(ests_hi_both_com, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -1, y = 0, label = label_expr), size = 4.5, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

com

## psychotic factor plot

# restrict to neu and create % labels
ests_hi_both_psy <- ests_hi_both_sub %>%
  filter(predictor=="psy") %>%
  mutate(perc_labels = scales::percent(prop,accuracy = 1))

# compute the cumulative percentages (top of each rectangle)
ests_hi_both_psy$ymax <- cumsum(ests_hi_both_psy$prop)

# compute the bottom of each rectangle
ests_hi_both_psy$ymin <- c(0, head(ests_hi_both_psy$ymax, n=-1))

# compute the size of each rectangle
ests_hi_both_psy$xmax <- ests_hi_both_psy$scaled_overall
ests_hi_both_psy$xmin <- ests_hi_both_psy$xmax-0.45

# compute label positions on both axes
ests_hi_both_psy$labelPosition.y <- (ests_hi_both_psy$ymax + ests_hi_both_psy$ymin) / 2
ests_hi_both_psy$labelPosition.x <- ests_hi_both_psy$xmax + 0.8

# r-squared
ests_hi_both_psy$overall_rounded <- round(ests_hi_both_psy$overall/2, 2)

# create the expressions for the labels
ests_hi_both_psy$label_expr <- paste0("R^2 == ", ests_hi_both_psy$overall_rounded)

# create donut chart correlated factor model (psychotic factor)
psy<-ggplot(ests_hi_both_psy, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=model)) +
  geom_rect() +
  geom_text(aes(y=labelPosition.y, x=labelPosition.x, label=perc_labels, color=model), size=4.5,show.legend = F) +
  geom_text(aes(x = -1, y = 0, label = label_expr), size = 4, parse = TRUE) + 
  scale_colour_manual(values= c("#0072B2","#D55E00")) +
  scale_fill_manual(values= c("#0072B2","#D55E00")) +
  guides(fill=guide_legend(NULL)) +
  coord_polar(theta="y",clip = 'off') +
  xlim(c(-1,4)) +
  theme_void() +
  theme(legend.position = "null")

psy

patch <- (plot_spacer() | plot_spacer() | p | guide_area() + plot_layout(guides="collect")) / (com | psy | neu | int)

patch
