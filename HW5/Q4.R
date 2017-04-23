density <- function(k, i, distanceMatrix){
  d_id <- distanceMatrix[i,]
  b <- sort(d_id)
  return(k/sum(b[2:(k+1)]))
}

nearestNeighbors <- function(k, i, distanceMat){
  d_id <- distanceMat[i,]
  b <- sort(d_id)
  return(as.numeric(names(b[2:(k+1)])))
}

centroids <- matrix(c(9,9,2,2),nrow = 2,ncol = 2)
delta0 <- matrix(runif(80,0,5),ncol=2)
delta1 <- matrix(runif(80,0,.9),ncol=2)

d0<- centroids[,1] - delta0
d1<- centroids[,2] + delta1

dataPoints <- rbind(d0,d1,c(1.25,1.25))

#dPoints <- dataPoints[-41,]
#dataPoints <- rbind(dPoints,c(5,7.5))
k <- 2

plot(dataPoints,xlim = c(0,10),ylim = c(0,10), col = c(rep("red", 40),rep("blue", 40),"green"))

distanceMatrix <- as.matrix(dist(dataPoints))
densityVector <- vector()
Traditional_Density_Score <- vector()
Relative_Density_Score <- vector()

for (i in 1:nrow(dataPoints)){
  densityVector[i] <- density(k, i, distanceMatrix)
  Traditional_Density_Score[i] <- densityVector[i]^(-1)
}

for(i in 1:nrow(dataPoints)){
  nns <- nearestNeighbors(k, i, distanceMatrix)
  a <- 0
  for (j in nns){
    a <- a + density(k, j, distanceMatrix)
  }
  Relative_Density_Score[i] <- densityVector[i]*k/a
  Relative_Density_Score[i] <- Relative_Density_Score[i]^(-1)
}

library(ggplot2)

qplot(dataPoints[,1], dataPoints[,2], colour=Relative_Density_Score,xlim = c(0,10),ylim = c(0,10),xlab = "",ylab = "") +
    geom_text(aes(label=round(Relative_Density_Score,digits = 2)),hjust=0.5, vjust=1.5,size=3)+
#  geom_point(aes(size = relativeDensityVector^(-1))) + 
  scale_colour_gradientn(colours = c("darkblue","green","red"),name ='Relative Density Score') +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey",size = 0.25),
        panel.grid.minor = element_line(colour = "grey",size = 0.25,linetype = "dotted"))

qplot(dataPoints[,1], dataPoints[,2], colour=Traditional_Density_Score,xlim = c(0,10),ylim = c(0,10),xlab = "",ylab = "") +
  geom_text(aes(label=round(Traditional_Density_Score,digits = 2)),hjust=0.5, vjust=1.5,size=3)+
  #  geom_point(aes(size = relativeDensityVector^(-1))) + 
  scale_colour_gradientn(colours = c("darkblue","green","red"),name ='Traditional Density Score') +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "darkgrey",size = 0.25),
        panel.grid.minor = element_line(colour = "grey",size = 0.25,linetype = "dotted"))

