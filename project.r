library(data.table)

dat <- data.table(read.csv('car.data'))
cols <- c('buying','maint','doors','people','trunk','safety','class')

setnames(dat, cols)

str(dat)
print(summary(dat))

# in order to do cluster analysis, we need to recode the levels and introduce
# metricization
for(v in cols)
  levels(dat[[v]]) <- 1:length(levels(dat[[v]]))

print(dat)

# Ward Hierarchical Clustering
d <- dist(dat, method = "euclidean") # distance matrix

fit <- hclust(d, method="ward")
plot(fit) # display dendogram

groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters

rect.hclust(fit, k=5, border="red")
