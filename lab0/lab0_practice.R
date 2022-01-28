#convert to character
numbers <- c(1,2,3,4,5)
as.character(numbers)

#matrices
#add row, columns
row1<- c(1,2,3,4)
row2 <- c(10,20,30,40)
mat1 <-cbind(row1, row2)
mat1
mat2 <-rbind(row1, row2)
mat2

mat3 <- mat2 %*% mat1
mat3

class(airquality)
airquality
dim(airquality)

colnames(airquality)
rownames(airquality)

airquality$Ozone

ls()

ls(pattern="ow")

rm(mat3)

attach(airquality)
colnames(airquality)
Ozone
detach()

airquality_mat <- as.matrix(airquality) #convert data frame airfares as a matrix
airquality_as_dataframe <- data.frame(airquality_mat) #convert back to data frame


# PDF probability distribution function
# cdf cummuative distribution function; area under the pdf from left to x

## normal distribution
#pdf dnorm()
#cdf pnorm()

x<-seq(-4,4,0.01)
hx<-dnorm(x,0,1)
par(mar=c(1,1,1,1))
plot(x, hx, type = "l", lwd=2, col="red", main = "Normal pdf")

px <- pnorm(x, mean=0, sd=1)
plot(x, px, type="l", lwd=2, col="green", main="Normal CDF")

# OR use function dnorm()
curve(dnorm(x), from=-4, to=+4, main="Normal pdf")
curve(pnorm(x), from=-4, to=4)


#check normal population using random sample
s.norm <-rnorm(100)
hist(s.norm) #histogram on frequency
hist(s.norm, probability = TRUE) #histogram on probability

#add ppoints using empirical density
points(density(s.norm), col="red", type='l')

#box plot
boxplot(s.norm)

#qq plot
qqnorm(s.norm)
qqline(s.norm)

#if points lie on the line, then data is normal

shapiro.test(s.norm)
#null hypothesis, the data is normal if p is greater than alpha value (0.05)

#chi-squares t, F sampling distribution
#X1, X2, X3.... Xm are independent random variables having standard normal distribution N(0,1)
# then V=X1^2.... Xm^2 follows a chisquare distribution
dchisq(x, df=5)
curve(dchisq(x, df=5), from=0, to=20, main="Chi-square Pdf, df=5,10,15")
curve(dchisq(x, 15), from=0, to=20, col="red", add=TRUE)

curve(pchisq(x, 5), from=0, to=20, col="green", main="Chisquare, df=5, CDF")

## Chi-square variable: create a sample Chisquare variable, degree freedom 5 
abline(a=10.6035, b= 0.5949) 
abline(a=10.6035, b= 0.5949, lwd=3, col="red")

z1 <- rnorm(100)
z2 <- rnorm(100)
z3 <- rnorm(100)
z4 <- rnorm(100)
z5 <- rnorm(100)
x.chisq <- z1^1 +z2^2 +z3^3+z4^2+z5^2
hist(x.chisq)
qqnorm(x.chisq)
qqline(x.chisq)
shapiro.test(x.chisq)

# W = 0.37514, p-value < 2.2e-16; null hypothesis fails, population is not normal


#Plot Normal pdf and t-pdf on same plot: use dt(); pt(); qt() 
curve(dnorm(x), from=-5, to=5, col="red", main="Standard Normal PDF") 
curve(dt(x, 10), from=-5, to=5, col="blue", add=TRUE)
curve(pt(x, 10), from=-5, to=5,lwd=2, col="red" ,main="t-dist, df=5 , CDF" )

## create a sample of t distribution : rt()
x.t <- rt(100, df=10)
qqnorm(x.t)
qqline(x.t)

# F distributino

x<- seq(0, 100,by=.1)
hf<- df(x, df1=10, df2=5)
plot(x, hf, type="l", lwd=2, col="red", main=" F(10, 5) pdf")

pf <- pf(x,df1=10,df2=5)
plot(x, pf, type="l", lwd=2, col="green", main="F cdf")
C. Important
