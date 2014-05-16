HopJas<-read.csv("/Users/Babydoll/Documents/FST/Haas/HopJason.csv", header = TRUE, stringsAsFactors = FALSE)
#head(HopJas)
# replace NAs with 0
#install.packages("qdap")
library(qdap)
??qdap
??NAer
HopJas<-NAer(HopJas, replace = 0)
head(HopJas)
HopJas<-HopJas[,1:18]
head(HopJas)
write.csv(HopJas,"/Users/Babydoll/Documents/FST/Haas/HopJas.csv")

HopTim<-read.csv("/Users/Babydoll/Documents/FST/Haas/HopTim.csv", header = TRUE, stringsAsFactors = FALSE)
HopJas<-read.csv("/Users/Babydoll/Documents/FST/Haas/HopJas.csv", header = TRUE, stringsAsFactors = FALSE)


#### combind the two data sets to one
head(HopTim)
dim(HopTim)
dim(HopJas)
head(HopJas)
hops<-rbind(HopTim, HopJas)

#install.packages("dplyr")
#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
str(hops)
####Need to convert character variables into numeric

hops$Cedar<-as.numeric(as.character(hops$Cedar))
hops$Citrus<-as.numeric(as.character(hops$Citru))
hops$Floral<-as.numeric(as.character(hops$Floral))
hops$Grassy<-as.numeric(as.character(hops$Grassy))
hops$Herbal<-as.numeric(as.character(hops$Herbal))
hops$Onion.G<-as.numeric(as.character(hops$Onion))
hops$Pine<-as.numeric(as.character(hops$Pine))
hops$Spicy<-as.numeric(as.character(hops$Spicy))
hops$St.Fruit<-as.numeric(as.character(hops$St.Fruit))
hops$Sweaty<-as.numeric(as.character(hops$Sweaty))
hops$Tob.Eart<-as.numeric(as.character(hops$Tob.Eart))
hops$Trop.Frt<-as.numeric(as.character(hops$Trop.Frt))

#Use dplyr to summarize by hop variety
hops_df<- tbl_df(hops)
hops_gr<-group_by(hops_df, Hop.ID )
sensmean<-summarize(hops_gr, 
                    mn_cedar = mean(Cedar),
                    mn_citrus = mean(Citrus),
                    mn_floral = mean(Floral),
                    mn_grassy = mean(Grassy),
                    mn_herbal = mean(Herbal),
                    mn_onion = mean(Onion.G),
                    mn_pine = mean(Pine),
                    mn_spicy = mean(Spicy),
                    mn_st.fruit = mean(St.Fruit),
                    mn_sweaty = mean(Sweaty),
                    mn_tobac = mean(Tob.Eart),
                    mn_trop.frt = mean(Trop.Frt))
round(sensmean[,2:13],2)
write.csv(sensmean,"/Users/Babydoll/Documents/FST/Haas/Sensmean.csv")

install.packages("mclust")
library(mclust)
?Mclust
fit <- Mclust(sensmean)
summary(fit)
plot(fit, "classification") # plot results 

install.packages('MVA')
library(MVA)
str(sensmean)

# may or may not be necessary to access the
# measure data

#
# We'll use the function hclust (from the stats package)
#

help(hclust)

#
# You have to pass hclust a distance matrix
#

help(dist)

#
# You have to pick a type of distance. Euclidean seems OK
#

sens <- sensmean[,2:13] # the chest, hip, waist measurements
ds <- dist(sens) # the distances between the rows

#
# take a look at the distances between the first 5 rows of
# data...notice that main diagonal contains zeros.
#

round(as.matrix(ds)[1:5,1:5],2)

#
# Now the clustering using single linkage
#
par( mfrow = c( 1, 1 ) ) 
hc.single <- hclust(ds,method="single")
plot(hc.single)

#
# using complete linkage 
#

hc.complete <- hclust(ds,method="complete")
plot(hc.complete)

#
# using average linkage
#

hc.average <- hclust(ds,method="average")
plot(hc.average)

#
# A few comments: (1) If you look at the measure data, the 
# first 10 observations are measurements of males, the last 
# 10 are measurements of females. (2) Single linkage often
# leads to long, "straggly" clusters; complete and average
# linkage are usually prefered.
#

#
# It can be useful to look at a principal components analysis
# in conjunction with the denograms.
#

pca <- prcomp(sens,scale=T)
summary(pca)
print(pca)

#
# get cluster labels from the complete linkage dendogram,
# and plot PC2 vs PC1
#

lab = cutree(hc.complete,h=10)
plot(pca$x[,1],pca$x[,2],"n",xlab="PC1",ylab="PC2")
text(pca$x[,1],pca$x[,2],labels=lab,cex=0.6,col=rep(c(1,2),rep(10,2)))

#
# black is male, red is female.
#

#
# K-means cluster for the same data, taking K=2
#

#
# First, check the variances of each measurement
#
?kmeans
apply(sens,2,var)
kmeans(sens,centers=2)
kmeans(scale(sens),centers=2)

#
# there doesn't seem to be much difference
h<-hclust(dist(sens, method = "euclidean"), method = "complete")
plot(h)

hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {    
  hclust(dist(x, method = dmeth), method = method)
}
?hclust
fit <- hclustfunc(sensmean)
plot(fit)

par(mfrow=c(1,1))
