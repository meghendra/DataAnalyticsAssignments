#C1
c1fprx <- c(0,0,0.2,0.2,0.2,0.4,0.4,0.6,0.8,0.8,1) 
c1tpry <- c(0,0.2,0.2,0.4,0.6,0.6,0.8,0.8,0.8,1,1)
#C2
c2fprx <- c(0,0,0,0,0,0.2,0.4,0.4,0.6,0.8,1) 
c2tpry <- c(0,0.2,0.4,0.6,0.8,0.8,0.8,1,1,1,1)
heading = "ROC Curve for classifiers C1 and C2" 
plot(x, y, type="n", xlab = "FPR", ylab = "TPR", 
     xlim = c(0,1), ylim = c(0,1),main=heading) 
lines(c1fprx, c1tpry, type="o", col="blue")
lines(c2fprx, c2tpry, type="o", col="red")
grid(lty="solid",lwd = 0.3 )
legend(0.8,0.2, c(": C1",": C2"), pch=c("o","o"), 
       lwd=c(1,1), col=c("blue","red"), bty = "o", 
       title = "Classifier")