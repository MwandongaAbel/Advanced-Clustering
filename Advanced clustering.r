#Advanced Clustering___________________Abel Mwandonga Haro
library(factoextra)
data("USArrests")
df<-scale(USArrests)
res_hk<-hkmeans(df,4)
# Elements returned by hkmeans()
names(res_hk)
res_hk
# Visualize the tree
fviz_dend(res_hk,cex = 0.6,palette = "jco",
          rect = TRUE,rect_border = "jco",
          rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(res_hk,palette="jco",repel=TRUE,
             ggtheme = theme_classic())

#Fuzzy Clustering
library(cluster)
hf<-scale(USArrests)
res_fanny<-fanny(hf,2)

# Membership coefficients
head(res_fanny$membership)
# Dunn partion coefficient
res_fanny$coeff
head(res_fanny$clustering)
fviz_cluster(res_fanny,ellipse.type = "norm",repel = TRUE,
             palette="jco",theme=theme_minimal(),
             legend="right")
#evaluate the goodnesss of the clustering results
fviz_silhouette(res_fanny,palette="jco",ggtheme=theme_minimal())

#Model based Clustering
install.packages("MASS")
library(MASS)
data("geyser")
head(geyser)
str(geyser)
# Scatter plot
library(ggpubr)
ggscatter(geyser,x="duration",y="waiting")+geom_density2d()

#Computing model-based clustering
install.packages("mclust")
library(mclust)
data("diabetes")
?diabetes
head(diabetes)
gf<-scale(diabetes[,-1])
mc<-Mclust(gf)
summary(mc)
mc$modelName
mc$G
head(mc$z,30)
head(mc$classification,30)

# BIC values used for choosing the number of clusters
fviz_mclust(mc,"BIC",palette="jco")
# Classification: plot showing the clustering
fviz_mclust(mc,"classification",geom="point",
            pointsize=1.5,palette="jco")
# Classification uncertainty
fviz_mclust(mc,"uncertainty",palette="jco")

#DBSCAN: Density-Based cclustering
data("multishapes")
dim(multishapes)
head(multishapes)
?multishapes
df<-multishapes[,1:2]
set.seed(123)
km_res<-kmeans(df,5,nstart = 25)
fviz_cluster(km_res,df,geom = "point",
             ellipse = FALSE,show.clust.cent = FALSE,
             palette="jco",ggtheme = theme_classic())
install.packages("dbscan")
library(dbscan)
library(factoextra)
library(fpc)
set.seed(123)
db<-fpc::dbscan(df,eps=0.15,MinPts = 5)
fviz_cluster(db,data=df,stand = FALSE,
             ellipse = FALSE,show.clust.cent = FALSE,
             geom = "point",palette="jco",
             ggtheme = theme_classic())
print(db)

#determining the optimal eps value
dbscan::kNNdistplot(df,k=5)
abline(h=0.15,lty=2)
