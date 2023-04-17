# Input Data
library(readxl)
data = read_excel('kemiskinan 2022.xlsx')
str(data)
colSums(is.na(data))
summary(data)

# Pre Processing
colnames(data) = c('provinsi','P1','P2','P0','TPT','AMH','RLS')
prov = c(data$provinsi)
data = data[,-1]
rownames(data) = prov
trans = scale(data, scale = TRUE)
jarak = dist(trans, method = 'euclidean')

# Pemilihan Metode Terbaik
library(tidyverse)
library(cluster)
m = c('single','complete','average','ward')
names(m) = m
ac = function(x){
  agnes(jarak, method=x)$ac
}
map_dbl(m,ac)

# Ward Clustering
ward = hclust(jarak,method = 'ward.D')
plot(ward,cex=1)
library(factoextra)
fviz_dend(ward, k = 5, k_colors = 'jco')
ward_clust = cutree(ward, k = 5)
head(ward_clust)
table(ward_clust)

# Hasil Clustering
tabel = data.frame(data,cluster=ward_clust)
karak = aggregate(tabel[,-7], list(tabel$cluster),mean)
colnames(karak)[1] = 'cluster'