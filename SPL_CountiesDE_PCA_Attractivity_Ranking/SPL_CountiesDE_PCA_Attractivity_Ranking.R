# -------------------------------------------------------------------------- Quantlet:
# Attractivity of regions - Rankings, Heatmaps, Clustering
# --------------------------------------------------------------------------

# setwd('C:/Users/Christian/Google Drive/Eigener_R_Projektordner')
setwd("./2.5_Attractivity_Ranking/")

dat = read.table("data.txt", header = TRUE, sep = "\t", dec = ",", na.strings = "NA", check.names = FALSE)
dat[, 4:10] = lapply(dat[, 4:10], function(x) as.numeric(as.character(x)))

# Loading relevant packages
if (!require("psych")) install.packages("psych")
library("psych")
if (!require("paran")) install.packages("paran")
library("paran")
if (!require("lattice")) install.packages("lattice")
library("lattice")


# -------------------------------------------------------------------------- 1. DATA
# PREPARATION --------------------------------------------------------------------------

# Delete superficial variables which are either not numerical or not part of the original
# dataset.
dat_pca1 = dat[, -c(1, 2, 3, 10, 11, 12, 13)]

#---------------------------------------------------------------------------
# 2. IS THE DATA SUITED FOR PCA/FA?
#---------------------------------------------------------------------------

# 2.1) Matrix-Plot
p = solve(cor(dat_pca1, use = "complete.obs"))
matplot(p, show.legend = T, axes = F)
# Result: The diagonal elements of the matrix lie above the non-diagonal elements. This
# indicates suitability of dataset for PC/FA.

# 2.2) Bartlett-Test of Sphericity
cortest.bartlett(dat_pca1)
# Result: P-Value is way below 0.05. The null hypothesis of the correlation matrix being the
# identity matrix can be rejected; the dataset is suitable.

# 2.3) What correlations might drive our principal comonent analysis?
pairs(dat_pca1, cex = 0.5, upper.panel = NULL)
# Interpretation: The plot implies correlations b/w most of the variables - exception: GDP
# growth rate.

# 2.4) Main indicator for suitability: KMO/MSA
KMO(dat_pca1)
# MSA for GDP-Growth-Rate is just below the 0.5 threshold (0.46). In a first attempt I keep
# it. After having conducted the first PCA, I repeat an alternative PCA w/o GDP growth rate.

#----------------------------------------------------------------------------------------
# 3. PCA2: WITHOUT GDP GROWTH RATE (MRA<0.5)
#----------------------------------------------------------------------------------------

# 3.1) Deleting GDP Growth Rate from the Dataset
dat_pca2 = dat_pca1[, -c(4)]

# 3.2) How does the suitability of Data change?
KMO(dat_pca2)
# The results show that operating w/o 'GDP growth rate' the KMO/MSA improve slightly. ALS
# TABELLE DARSTELLEN F?R DIE SEMINARARBEIT


# 3.3) How many Principal Components need to be retained?
scree(dat_pca2)
# Result: One-Component-Model GRAFIK IN SEMINARARBEIT ?BERNEHMEN

# 3.4) Conducting the PCA
pca2 = principal(dat_pca2, nfactors = 1, scores = TRUE, rotate = "none")
pca2
# Results show that most loadings improve slightly compared to the previous model. TABELLE
# IN DIE SEMINARARBEIT

#---------------------------------------------------------------------------------
# 4. EXTRACTION OF SCORES
#---------------------------------------------------------------------------------

# 4.1)extracting scores of pca2
pca2_scores = pca2$scores
# pca2_scores

# 4.2)Linking Scores to regions
dat_attract = data.frame(dat$Name, dat$Bundesland, pca2_scores)
View(dat_attract)


# Simon: Ranking, Heatmap, Comparison with 'Gl?cksatlas 2011', evtl. Clustering
# head(dat_attract) sort regions in descending order with respect to the pca2 score
dat_attract = dat_attract[order(-dat_attract[, 3]), ]

# -------------------------------------------------------------------------- 5. Data
# analysis --------------------------------------------------------------------------

# ===================== 5.1) Print score table =====================
if (!require("xtable")) install.packages("xtable")
library("xtable")
library("psych")

nb_obsv = 6  # number of obersvations we are interested in

# get the first and last six entries of the data set.
score_table = headTail(dat_attract, hlength = nb_obsv, tlength = nb_obsv, digits = 4, ellipsis = TRUE)

# generate ellipsis, in LaTeX style
dots = data.frame(c("\\vdots"), c("\\vdots"), c("\\vdots"))
names(dots) = colnames(score_table)

# replace default ellipsis
score_table = rbind(score_table[1:nb_obsv, ], dots, score_table[(nb_obsv + 2):(nb_obsv * 2 + 
    1), ])

# generate LaTeX table and add header and row names
score_table = xtable(score_table)
names(score_table) = c("City", "State", "PC2 Score")
row.nr = dim(dat_attract)[1]
row.names(score_table) = c(1:nb_obsv, "\\vdots", (row.nr - (nb_obsv - 1)):row.nr)

# print the table, and copy and past it into the LaTeX document. We have to use the
# sanitize.text.function to keep all backslashes, to run the LaTeX code.
str_caption = "The six best and worst regions, according to their PC2 score."
print(xtable(score_table, caption = str_caption), sanitize.text.function = function(x) x)

if (TRUE) {
    # delete variables, which we will no longe use. Change to FALSE, to keep them.
    rm(dots, str_caption, row.nr, score_table)
}

# ===================== 5.2) Generate heatmap =====================

# if not already exists, install sp package to plot the heatmap
if (!require("sp")) install.packages("sp")
library(sp)

# download German federal states as polygons Source:
# http://biogeo.ucdavis.edu/data/gadm2.8/rds/DEU_adm1.rds
hmap_score = readRDS("./DEU_adm1.rds")

# We use 'ue' instead of 'ü' in our data. Because of that we have to replace them in the
# heatmap object, to be able to match our data with the given data structure.
hmap_score@data$NAME_1 = gsub("ü", "ue", hmap_score@data$NAME_1)

# get all levels (different states) in our data frame
dat_attract_levels = levels(dat_attract$dat.Bundesland)

# new data frame to store the normalized score of each state
state_scores = data.frame(dat_attract_levels, 0)
names(state_scores) = c("NAME_1", "score")

# exclude states without enough data
exclude = c("Bremen", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt")
state_scores = subset(state_scores, !(state_scores$NAME_1 %in% exclude))


for (i in 1:dim(state_scores)[1]) {
    # collect all regions of one state
    state = dat_attract[dat_attract$dat.Bundesland == state_scores[i, 1], 3]
    state_scores[i, 2] = sum(state)/length(state)
}

# merge heatmap structure and our data
hmap_score@data = data.frame(hmap_score@data, state_scores[match(hmap_score@data[, "NAME_1"], 
    state_scores[, "NAME_1"]), ])

# plot the heatmap
spplot(hmap_score, zcol = "score", main = "Region Attractiveness", col.regions = heat.colors(50)[10:40])

if (TRUE) {
    # delete variables, which we will no longe use. Change to FALSE, to keep them.
    rm(dat_attract_levels, hmap_score, state_scores, state, exclude)
}
# -------------------------------------------------------------------------- 6. Data
# clustering --------------------------------------------------------------------------

# ===================== 6.1) Search cluster =====================

# only select colums with nummeric values for k-algorithm. => We selected them already in
# line 9.
summary(dat_pca1)

# -------- 6.1.1) Prepare data --------

# custom function to standardize the data. The k-means clustering algorithm is sensitive to
# differences in scale and variance of the variables. Therefore the data should be
# standardized before clustering.
standardize = function(x) {
    mu     = mean(x)
    std    = sd(x)
    result = (x - mu)/std
    return(result)
}

# copy the data, to keep the original ... also reorder dat_attract to match it with score
# data later.
helper = dat_attract[order(as.numeric(row.names(dat_attract))), ]
dat_cluster = cbind(dat_pca1, pca_score = c(helper$PC1))
rm(helper)  # we don't need this any longer

# standardize all coloumns. -1 because the PC1 is already standardized.
for (i in 1:(dim(dat_cluster)[2] - 1)) {
    dat_cluster[, i] = standardize(dat_cluster[, i])
}

# -------- 6.1.2) Run k-means algorithm --------

# maximal number of cluster center
k = 15
# vector with a range of clusters, we will test
k_settings = 1:k

# vector to store the result of each run of the k-mean algorithm
clu_values = vector(mode = "numeric", length = length(k_settings))

# vector to store the complet cluster of each run
clu_models = vector(mode = "list", length = length(k_settings))

# run the k-means
for (i in 1:length(k_settings)) {
    # Create a cluster solution using the current value of k
    clu_sol         = kmeans(dat_cluster, centers = k_settings[i], iter.max = 50, nstart = 100)
    
    # save only the total sum of squares per cluster of current run
    clu_values[i]   = clu_sol$tot.withinss
    
    # save the full cluster model of the current run
    clu_models[[i]] = clu_sol
}

# -------- 6.1.3) Plot result of k-means --------

# plot the result pdf('2_elbow.pdf')
par(mar = c(5, 5, 14, 5))
plot(k_settings, clu_values, xlab = "k cluster", ylab = "Total within-cluster sum of squares", 
    main = "Elbow curve for k cluster", col = "red", type = "b")
# dev.off() ==> according to the elbow curve we choose three cluster

if (TRUE) {
    # delete variables, which we will no longe use. Change to FALSE, to keep them.
    rm(clu_sol, clu_values, k_settings)
}

# ===================== 6.2) Analyse cluster =====================

# get the model with three cluster
clu_model = clu_models[[3]]
rm(clu_models)  # we don't need the other models anymore

# -------- 6.2.1) Plot number of regions per cluster --------

# get the indices of each case, in respect to the cluster it belongs to
clu_1_data = as.numeric(names(clu_model$cluster[clu_model$cluster == 1]))
clu_2_data = as.numeric(names(clu_model$cluster[clu_model$cluster == 2]))
clu_3_data = as.numeric(names(clu_model$cluster[clu_model$cluster == 3]))

# total numbers of regions belong to one cluster
regions_per_clu = c(length(clu_1_data), length(clu_2_data), length(clu_3_data))

# pdf('3_number_of_regions.pdf')
color_bars = c("#FFFF00FF", "#FF8000FF", "#FF0000FF")
plot = barplot(height = regions_per_clu, width = c(0.3), horiz = TRUE, ylim = c(0, 3), names.arg = c("1", 
    "2", " 3"), xlab = "Number of Regions", xlim = c(0, max(regions_per_clu) + 26), col = color_bars, 
    border = NA)  #, main = 'Total Number of Regions per Cluster')

# add number of observations on top of each bar
text(y = plot, x = regions_per_clu, label = regions_per_clu, pos = 2, cex = 0.8)
title(ylab = "Cluster", line = 3, cex.lab = 1)
# dev.off() -------- 6.2.2) Print table of cluster center --------

if (!require("xtable")) install.packages("xtable")
library("xtable")

# edit row and column names
matr_centers = clu_model$centers
rownames(matr_centers) = c("Cluster 1", "Cluster 2", "Cluster 3")
colnames(matr_centers) = c(colnames(matr_centers)[1:6], "PCA Score")  # just change the last

# notice, we use t() to transpose the center matrix. It will better fit the page width in
# our LaTeX document later.
str_caption = "Cluster Center"
t(matr_centers)
print(xtable(t(matr_centers), caption = str_caption))

# -------- 6.2.3) Plot how the different states are split up in the cluster --------

# match region and state with cluster number
dat_cluster_final = cbind(dat[, 1:2], Cluster = clu_model$cluster)

# exclude states without enough data exclude = c('Bremen', 'Mecklenburg-Vorpommern',
# 'Sachsen', 'Sachsen-Anhalt') dat_cluster_final = subset(dat_cluster_final,
# !(dat_cluster_final$Bundesland %in% exclude))

clu_1_states = dat_cluster_final[dat_cluster_final$Cluster == 1, 2]
clu_2_states = dat_cluster_final[dat_cluster_final$Cluster == 2, 2]
clu_3_states = dat_cluster_final[dat_cluster_final$Cluster == 3, 2]

# combine summery of each cluster. This gives us an overview how many regions are in each
# cluster
matr_states_cluster = cbind(as.matrix(summary(clu_1_states)), as.matrix(summary(clu_2_states)), 
                            as.matrix(summary(clu_3_states)))
matr_states_cluster = matr_states_cluster[-c(4, 6, 11, 12), ]  # 6 11 12

colnames(matr_states_cluster) = c("Cluster 1", "Cluster 2", "Cluster 3")

# calculate ration
matr_states_cluster = prop.table(matr_states_cluster, margin = 1)

# pdf('3_stacked_bar.pdf') setup the plot
par(mar = c(4, 13, 3, 5))  # set margin of plot
# set color for bars color_bars = heat.colors(3)#[10:40]#c('#8DBC56','#568DBC', '#526D84')
color_bars = c("#FFFF00FF", "#FF8000FF", "#FF0000FF")  #c('#FF8000FF','#FFFF00FF','#FF0000FF')

barplot(t(matr_states_cluster), horiz = TRUE, las = 1, col = color_bars, xlab = "Ratio of cluster in each region", 
    border = NA)

par(xpd = TRUE)  # enable clipping 
# add legend outside of the plot
legend(0.1, 13.2, c("cluster 1", "cluster 2", "cluster 3"), fill = color_bars, box.col = "white", 
    horiz = TRUE, text.width = 0.2, border = NA)

## dev.off()

if (TRUE) {
    # delete variables, which we will no longe use. Change to FALSE, to keep them.
    rm(str_caption, matr_centers, clu_1_data, clu_2_data, clu_3_data, clu_1_states, clu_2_states, 
        clu_3_states, plot, color_bars)
}

