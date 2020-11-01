# ---- Normal Dist ----

set.seed(1977)

n = 1000

df.rand = NULL
for(i in 1:n){
  tmp = rnorm(n = 100, mean = 1, sd =1)
  if(is.null(df.rand)){
    df.rand = tmp
  } else {
    df.rand = cbind(df.rand,tmp)
  }
}


df.mean = apply(X = df.rand,2,FUN = mean)

hist(df.mean)


# ---- Discrete Dist ----
library(distr)
newDist = DiscreteDistribution(supp = c(0,1,2,3,4,5,6),prob = c(.08,.1,.3,.1,.1,.12,.2))

set.seed(1977)

n = 1000

df.dist = NULL
for(i in 1:n){
  tmp = r(newDist)(100)
  if(is.null(df.dist)){
    df.dist = tmp
  } else {
    df.dist = cbind(df.dist,tmp)
  }
}


df.mean = apply(X = df.dist,2,FUN = mean)

hist(df.mean)



### --- Git changes ---- ####

