library(data.table)

dat <- data.table(read.csv('car.data'))

#Georgi:by opening it like this we loose the first observation. I propose 
#cdata <- data.table(read.csv('car.data', header=FALSE))

cols <- c('buying','maint','doors','people','trunk','safety','class')



setnames(dat, cols)

str(dat)
print(summary(dat))

# in order to do cluster analysis, we need to recode the levels and introduce
# metricization
for(v in cols)
  levels(dat[[v]]) <- 1:length(levels(dat[[v]]))
  
# Georgi: After metricization some conserns arise:  The realizations are neither ordered or comparable. I mean:
# "buying": 4-vhigh; 1-high, 3-med, 2-low; the same for "maint"; "safety": 2-low, 3-med, 1-high. Let us consider some
# unified base structure for the analysis of each one of us. 
# I think application of PCA or FA should also make sense.
# ah and sth more - the matrix for cluster anal. should be standarized. I suppose "method = euclidean" takes this into account?
  

print(dat)

# Ward Hierarchical Clustering
d <- dist(dat, method = "euclidean") # distance matrix

fit <- hclust(d, method="ward")
plot(fit) # display dendogram

groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters

rect.hclust(fit, k=5, border="red")
