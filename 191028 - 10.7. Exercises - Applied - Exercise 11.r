# On the book website, www.StatLearning.com, 
# there is a gene expres-
#   sion data set ( Ch10Ex11.csv ) that 
# consists of 40 tissue samples with
# measurements on 1,000 genes. The 
# first 20 samples are from healthy
# patients, while the second 20 are 
# from a diseased group.
# 
# 
# (a) 
# 
# Load in the data using read.csv(). 
# You will need to select
# header=F .

?read.csv
csv = read.csv(Ch10Ex11.csv, header = F)

# (b) 
# 
# Apply hierarchical clustering to 
# the samples using correlation-
#   based distance, and plot the dendrogram. 
# Do the genes separate
# the samples into the two groups? 
#   Do your results depend on the
# type of linkage used?

x = Ch10Ex11
dim(x)
dd = as.dist(1-cor(x))  
plot(hclust(dd, method="complete"), main="Complete Linkage
     with Correlation-Based Distance", xlab = "", sub="")
plot(hclust(dd, method="average"), main="Average Linkage
     with Correlation-Based Distance", xlab = "", sub="")
plot(hclust(dd, method="single"), main="Single Linkage
     with Correlation-Based Distance", xlab = "", sub="")
  
#   (c) 
# 
# Your collaborator wants to know 
# which genes differ the most
# across the two groups. Suggest 
# a way to answer this question,
# and apply it here.