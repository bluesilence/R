require(BHH2)
x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
dotPlot(x)
mean.x <- mean(x)
sd.x   <- sd(x)
lines(rep(mean.x         , 2), c(0.2, 0.25)) # Vertical line at mean
lines(rep(mean.x + 2*sd.x, 2), c(0.2, 0.25)) # Vertical line at mean + 2 SD
text(mean.x         , 0.3, expression(bar(x)))
text(mean.x + 2*sd.x, 0.3, expression(paste(bar(x), " + 2s")))
