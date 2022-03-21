DrillSpeed.data<- c(125, 125, 200, 200, 125, 125, 200, 200, 125, 125, 200, 200, 125, 125, 200, 200)

FeedRate.data <- c(0.015, 0.015, 0.015, 0.015, 0.03, 0.03, 0.03, 0.03, 0.045, 0.045, 0.045, 0.045, 0.06, 0.06, 0.06, 0.06)

Force<- c(2.7, 2.78, 2.83, 2.86, 2.45, 2.49, 2.85, 2.8, 2.6, 2.72, 2.86, 2.87, 2.75, 2.86, 2.94, 2.88) 
results <- data.frame(DrillSpeed.data, FeedRate.data, Force)
results$DrillSpeed <- as.factor(results$DrillSpeed.data)
results$FeedRate <- as.factor(results$FeedRate.data)
str(results)
attach(results)
force.aov <- aov(Force~FeedRate*DrillSpeed)
summary.aov(force.aov)
library(rsm)
results.rsm<- rsm(Force ~ SO(FeedRate.data, DrillSpeed.data)) 
summary(results.rsm)
canonical(results.rsm)

#xs <-canonical(results.rsm)$xs
contour(results.rsm,~FeedRate.data+DrillSpeed.data, at=xs, image=TRUE)
detach()