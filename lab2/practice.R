A <- c(7, 7, 15, 11, 9)
B <- c(12, 17, 12, 18, 18)
C <- c(14, 18, 18, 19, 19)
D <- c(19, 25, 22, 19, 23)
E <- c(7, 10, 11, 15, 11)

temperature <- c(A, B, C, D, E)
trt<- rep(c("A", "B", "C", "D", "E"), each=5)
trt<-factor(trt)
trt.means<-tapply(temperature, trt, mean)
#Temperature <-cbind(trt, temperature)
Temperature <-data.frame(cbind(trt, temperature))

stripchart(temperature~trt, vertical=TRUE, pch=16, main="Stripchart")
lines(trt.means)


boxplot(temperature~trt, main="Box plot")
####################


model <-aov(temperature ~trt)

summary.aov(model)

summary.lm(model)
