y1<- c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
y2 <- c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
df = data.frame(cbind(y1, y2))
names(df)<- c("Modified", "Unmodified")
attach(df)
means <- apply(df,2,mean)

stripchart(df, xlab="Strength", vertical=TRUE, pch=16)
lines(means)

boxplot(df)

methods<- rep(c("Modified", "Unmodified"), each=10)
strength<- c(y1,y2)
# combining vectors gives a matrix, not a data frame.
data.1 <-data.frame( cbind(methods, strength))

#two sample ztest
library("BSDA")
#check if the means of two samples are same
z.test(y1, y2, sigma.x=.1, sigma.y=0.05, conf.level=0.95)


#check if the means of two samples are same
# using t-test
t.test(strength ~ methods, var.equal=TRUE) 


## 4

x1<- c(7 , 3 , 3, 4, 8, 3, 2, 9, 5, 4)
x2 <-c(6, 3, 5, 3, 8, 2, 4, 9, 4, 5)
measurement<-c(x1, x2)
type <- rep(c("tip1", "tip2"), each=10)
hardness<- data.frame(cbind(type, measurement))
t.test(measurement~type, pair=TRUE)

#variance test to check if the variance of the two samples is same
bartlett.test(data)

var.test(y1,y2)

bartlett.test(df)


# all tests are based on the fact that the data is normal

sample1<- rnorm(100) # sample of 100 from Normal population
qqnorm(sample1)
qqline(sample1)
wilcox.test(y1, y2)


#for uniform data, qq plot won't predict normal data
sample2 <-runif(100) # Sample of 100 from uniform population)
qqnorm(sample2)
qqline(sample2)

# F-distribution PDF
## F random sample; rf(n, df1, df2)
x.seq <- seq(0, 10, by=0.1)
x.seq
f.dist <- df(x.seq, 10,2)
plot(x.seq, f.dist, type="l",lwd=2, col=2)
stud.t.dist <-dt(x.seq, 20, 2)
points(x.seq, stud.t.dist, type="l",lwd=2, col=3)

x.chisq <- rchisq(50, 10)
y.chisq <- rchisq(50, 5) # 50 random form chisquare, df=5

f.sample <- (x.chisq/10)/(y.chisq/5)

hist(f.sample, probability = TRUE)
points(density(f.sample), type="l", col="red")
