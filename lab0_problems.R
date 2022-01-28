
#1(a) Generate a random sample of 100 from t-distribtion, degree of freedom 10. 
# Check qqnorm(); qqline(), Shapiro test. Remarks,

sample.t <- rt(100, df=10)
qqnorm(sample.t)
qqline(sample.t)
shapiro.test(sample.t)

#null hyposthesis that the values are normal is accepted

# 1.(b) Generate a random sample of 100 from a Chi-square distribution, df =5. 
#Perform same procedures as in (a) . Remarks.
sample.chi<-seq(-4,4,0.01)
dchisq(sample.chi, df=5)
qqnorm(sample.chi)
qqline(sample.chi)
shapiro.test(sample.chi)

#null hyposthesis that the values are normal is rejected pvalue <0.05

#2.
#(a) Write a 95%-CI for the population mean. 
#What assumption about population for the work, suppose the sample is random.

sample <-c(26.4,23.5,25.4,22.9,25.2,39.2,25.5,31.9,26.0,44.6,35.5,38.6,
           30.1,31.0,30.8,32.8,47.7,39.1,55.3,50.7,73.8,71.1,68.4,77.1,
           19.4,19.3,18.7,19.0,23.2,21.3,23.2,19.9,18.9,19.8,19.6,21.9)
hist(sample)
n<-length(sample)
xbar<-mean(sample)
s<-sd(sample)
margin <- qt(0.95,df=n-1)*s/sqrt(n)
low <- xbar - margin
low

high <- xbar + margin
high

#(b) Write a 95%- CI for population standard deviation.



#3. Quantile-Quantile (QQ) Plot
#Run the following code
sample <-c(26.4,23.5,25.4,22.9,25.2,39.2,25.5,31.9,26.0,44.6,35.5,38.6,
           30.1,31.0,30.8,32.8,47.7,39.1,55.3,50.7,73.8,71.1,68.4,77.1,
           19.4,19.3,18.7,19.0,23.2,21.3,23.2,19.9,18.9,19.8,19.6,21.9)
hist(sample)
sample.s <-sort(sample) #sort data increasing
rank <- rank(sample.s) #rank data from 1 to 36
size <- length(sample.s) # size of data
p <- (rank-.5)/size #cummulative prob of data
z.quantile <- qnorm(p) # Standard Normal quantiles with such probability
plot(x=z.quantile, y=sample.s, pch=16, main="QQ Plot") #scatterplot of x=Z quantiles, y= data sorted
abline(lm(sample.s ~ z.quantile))
#The QQ plot suggests that the data is not normal
