# This deals with similarity <-> distance measures of categorical data.
# Three types of similarity measures are discussed - overall, eskin and iof

library(data.table)

dat  <- data.table(read.csv('car.data', header=FALSE))
cols <- c('buying','maint','doors','people','trunk','safety','class')
setnames(dat, cols)

# takes a data.frame with factors
# returns value-frequency data.frame
freq <- function(dat)
{
  sapply(colnames(dat), function(field) as.matrix(table(dat[[field]])))
}

# takes an attribute, it's value and a data.frame
# returns `value`'s occurrences-count in `data`'s `attribute`
fkx <- function(attribute, value, dat)
{
  freq(dat)[[attribute]][value,]
}

# calculate overlap similarity between rows x and y
overlap.sim <- function(x, y, dat)
{
  d <- length(x)

  res = 0
  for (level in colnames(x))
    if (x[[level]] == y[[level]])
      res = res + 1

  res / d
}

# calculate eskin similarity between rows x and y
eskin.sim <- function(x, y, dat)
{
  attrs <- colnames(x)
  d     <- length(x)
  res   <- 0

  for (attrb in attrs)
    if (x[[attrb]] == y[[attrb]]) {
      res = res + 1
    } else {
      nk = length(levels(x[[attrb]]))
      res = res + nk**2 / (nk**2 + 2)
    }

  res / d
}

# calculate IOF similarity between rows x and y
iof.sim <- function(x, y, dat)
{
  attrs <- colnames(x)
  d     <- length(x)

  res = 0
  for (attrb in attrs)
    if (x[[attrb]] == y[[attrb]]) {
      res = res + 1
    } else {
      val = fkx(attrb, x[[attrb]], dat)
      if (val > 0)
        res = res + 1/(1 + log(val)*log(val))
    }

  as.vector(res / d)
}

# takes similarity and applies dist = (1-sim)/sim
to.dist <- function(sim)
{
  (1-sim)/sim
}

# This is only here for completion. This implementation takes ~ hours to run on
# the ~ 1800 entries, so it will have to be sped up.
#
# Example:
#   similarity.matrix('overall', dat)
#
similarity.matrix <- function(similarity, dat)
{
  N    <- nrow(dat)
  func <- get(paste(similarity, '.sim', sep=''), mode='function', envir=.GlobalEnv)

  matrix(combn(1:N, 2, function(x) func(dat[x[1],], dat[x[2],], dat)), N, N,
         byrow=TRUE)
}

# Apply the measures above to elements of the dataset at hand
x = dat[1,]
y = dat[2,]

cat('Two elements of the data:\n')
print(x)
print(y)

measures <- c('overlap', 'eskin', 'iof')
sapply(measures, function(measure){
  func <- get(paste(measure, '.sim', sep=''), mode='function', envir=.GlobalEnv)
  s    <- func(x, y, dat)

  cat('\n', measure, 'similarity: \n')
  print(s)
  cat('\n', measure, 'distance: \n')
  print(to.dist(s))
})

cat('\n IOF similarity matrix on head(data)')
print(similarity.matrix('iof', head(dat, 3)))

cat('\n *** END of similiarity measures section *** \n\n')
