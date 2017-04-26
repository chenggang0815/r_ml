
# =====================k-means======================================
newiris <- iris
newiris$Species <- NULL
(kc <- kmeans(newiris, 3)) 
table(iris$Species, kc$cluster)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)
# ==========visualization===========================================
library('ggfortify')
autoplot(kmeans(newiris, 3),data=newiris,label=TRUE, label.size=3, frame=TRUE)

#=============
#generate toy data with factors and numerics
n   <- 100
prb <- 0.9
muk <- 1.5
clusid <- rep(1:4, each = n)
x1 <- sample(c("A","B"), 2*n, replace = TRUE, prob = c(prb, 1-prb))
x1 <- c(x1, sample(c("A","B"), 2*n, replace = TRUE, prob = c(1-prb, prb)))
x1 <- as.factor(x1)
x2 <- sample(c("A","B"), 2*n, replace = TRUE, prob = c(prb, 1-prb))
x2 <- c(x2, sample(c("A","B"), 2*n, replace = TRUE, prob = c(1-prb, prb)))
x2 <- as.factor(x2)
x3 <- c(rnorm(n, mean = -muk), rnorm(n, mean = muk), rnorm(n, mean = -muk), rnorm(n, mean = muk))
x4 <- c(rnorm(n, mean = -muk), rnorm(n, mean = muk), rnorm(n, mean = -muk), rnorm(n, mean = muk))
x <- data.frame(x1,x2,x3,x4)
x
# apply k prototyps
kpres <- kproto(x, 4)
clprofiles(kpres, x)
# in real world  clusters are often not as clear cut
# by variation of lambda the emphasize is shifted towards factor / numeric variables
kpres <- kproto(x, 2)
clprofiles(kpres, x)
kpres <- kproto(x, 2, lambda = 0.1)
clprofiles(kpres, x)
kpres <- kproto(x, 2, lambda = 25)
clprofiles(kpres, x)


