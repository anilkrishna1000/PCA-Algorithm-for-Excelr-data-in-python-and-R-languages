View(wine)
attach(wine)
wine5=scale(wine)
## To check any missing values, hence no missing value present in the whole dataset.

any(is.na(wine)) # false , no NA  vales in our data set
dim(wine) # how many rows and columns we can see

##### Building PCA Summary ############
pcaObj1<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
?princomp

summary(pcaObj1)
## Observation:
## As per the summary above (Importance of components); the first 7 variables contribte ~90% of the information required for the entire data. Hence the 13 components can be reduced to 14 for furhter analysis with 90% information. The other variables can be included in case we intend to have more accurate analysis/forcasting/prediction.
## (see the plot(pcaobj1 line we get to know the 7 variables contribute the 90% information))
loadings(pcaObj1)


plot(pcaObj1) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

pcaObj1$scores # this is to check the scores of your principal components

pcaObj1$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata1
mydata1<-cbind(wine,pcaObj1$scores[,1:3])
View(mydata1)


# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data1<-mydata1[,15:17]
clus_data1

# Normalizing the data 
norm_clus1<-scale(clus_data1) # Scale function is used to normalize data
dist2<-dist(norm_clus1,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance


# Clustering the data using hclust function --> Hierarchical
fit2<-hclust(dist2,method="complete") # method here is complete linkage

#### Displaying Dendrogram for only the PCA variables #######
plot(fit2) 
plot(fit2, hang=-1)# Here hsng =-1 in order to arrange clustering numbers in single sequence line
groups1<-cutree(fit2,3) # Cutting the dendrogram for 5 clusters
rect.hclust(fit2, k=7, border="blue")

membership_2<-as.matrix(groups1) # cluster numbering 

View(membership_2)

final2<-cbind(membership_2,mydata1) # binding column wise with orginal data
View(final2)
View(aggregate(final2[,-c(2,16:18)],by=list(membership_2),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wines data on membership_2

##### K Means Clustering ####
library(plyr)
km <- kmeans(dist2,3) #kmeans clustering
str( km) ## Betweenness sum of squre is 10072 and 
## withinness sum of square is 14851


library(animation)
km <- kmeans.ani(wine, 3)

# selecting K for kmeans clustering using kselection
library(kselection)
k <- kselection(dist2,k_threshold = 0.9, )
k

##### elbow curve to decide the K Value ####

wss = (nrow(wine5)-1)*sum (apply(wine5, 2, var))# Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(wine5, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


## Observation(s):
   ### When PCA was applied on the entire set of varibles (14); PCA suggested that 90% of the information can be inferred from the first 7 varaibles. We then plotted dendrogram for both 14 varaibles and 7 varaibles data and found that the number of clustered required are 7 and the dendrogram seem identical.
