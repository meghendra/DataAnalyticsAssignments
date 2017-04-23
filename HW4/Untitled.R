x<- c(0.1,1,1.7,3.4,3.9,4.7)
d<- dist(x,method = "euclidean")
hc <- hclust(d,method = "single")
plot (hc)
plot(hc, labels = c(0.1, 1, 1.7, 3.4, 3.9, 4.7), xlab = "Data")
plot(as.dendrogram(hc), ylim = c(0,2.0))
axis(1, at=1:6, labels=c(0.1, 1, 1.7, 3.4, 3.9, 4.7))

y <- matrix(c(0,2,0,0,2,0,1,2,2,1,0,2,2,2,1,0),nrow=4,ncol=4)
#t(y)
require(lsa)
c <- cosine(y)
c1 <- 1 - c
hc1 <- hclust(as.dist(c1),method = "single")
plot(hc1, labels = c("A","B","C","D"), xlab = "Data")
hc1$order
hc1$height
1 - hc1$height

z <- matrix(c(1,.1,.41,.55,.35,.1,1,.64,.47,.98,.41,.64,1,.44,.85,.55,.47,.44,1,.76,.35,.98,.85,.76,1),nrow = 5,ncol = 5) 
z1 <- 1 - z
hc2 <- hclust(as.dist(z1),method = "single")
hc3 <- hclust(as.dist(z1),method = "complete")
plot(hc2, labels = c("p1","p2","p3","p4","p5"), xlab = "Data")
plot(hc3, labels = c("p1","p2","p3","p4","p5"), xlab = "Data")