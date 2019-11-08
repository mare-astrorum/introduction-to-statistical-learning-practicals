# Consider the USArrests data. We 
# will now perform hierarchical clus-
#   tering on the states.

library(ISLR)
set.seed(2)

# (a) 
# 
# Using hierarchical clustering 
# with complete linkage and
# Euclidean distance, cluster the 
# states.

hc.complete = hclust(dist(USArrests), method = "complete")
plot(hc.complete)

 
# (b) 
# 
# Cut the dendrogram at a height 
# that results in three distinct
# clusters. Which states belong 
# to which clusters?

hc.clusters = cutree(hc.complete, 3)
hc.clusters
table(cutree(hc.complete, 3))

#   (c) 
# 
# Hierarchically cluster the states 
# using complete linkage and Eu-
#   clidean distance, after scaling 
# the variables to have standard de-
#   viation one.

sd.data = scale(USArrests) 

hc.complete2 = hclust(dist(sd.data), method = "complete")
plot(hc.complete2)

 
# (d) 
# 
# What effect does scaling the 
# variables have on the hierarchical
# clustering obtained? In your 
# opinion, should the variables be
# scaled before the inter-observation 
# dissimilarities are computed?
# Provide a justification for your answer.

table(cutree(hc.complete2, 3))

table(cutree(hc.complete2, 3), cutree(hc.complete, 3))


# Scaling the variables effects the max height of 
# the dendogram obtained from hierarchical clustering. 
# From a cursory glance, it doesn’t effect the 
# bushiness of the tree obtained. However, it 
# does affect the clusters obtained from cutting 
# the dendogram into 3 clusters. In my opinion, 
# for this data set the data should be standardized 
# because the data measured has different units 
# (UrbanPop compared to other three columns).
# 
# Data should be scaled because the variances
# of the data sets are quite different.