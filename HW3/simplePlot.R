x <- c(1:5); y <- x
heading = "ROC Curve for classifiers C1 and C2" 
plot(x, y, type="n", main=heading) 
lines(x, y, type="o")