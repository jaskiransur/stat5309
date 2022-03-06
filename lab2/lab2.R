
### a. Set up the data frame.

commercial_plastic_wrap <- c(7.66, 6.98, 7.80)
vacuum_packaged <- c(5.26, 5.44, 5.80)

#1% CO,40% O2, 59% N
co_o2_n<- c(7.41, 7.33, 7.04)
co2_100_percent <-c(3.51, 2.91, 3.6)

logcount <- c(commercial_plastic_wrap, vacuum_packaged, co_o2_n, co2_100_percent)

package <- rep(c("comm_pwrap", "vacuum_pkd", "co_o2_n", "co2"),
              each=3)
package <-factor(package)
bacteria  <-data.frame(cbind(logcount, package))




### b. Perform a stripchart, with line connecting means, of logcount vs package

stripchart(logcount~package, vertical=TRUE, pch=16, main="Stripchart")

logcount.means<-tapply(logcount, package, mean)
lines(logcount.means)


### c. Build a linear model, using aov() response as logcount. Do a summary.lm() and 
model<- aov(logcount ~ package)
summary.aov(model)
summary.lm(model)


### d. Perform a Bartlett test of equal variances.
logcount.aov<- aov(model)
res <- residuals(logcount.aov)
qqnorm(res, main="normality")
qqline(res)
bartlett.test(res~package)

### e. Perform a multiple comparison of treatment mean, using TukeyHSD()
tuskey <-TukeyHSD(model, "package")
tuskey
plot(tuskey)


## Q.2. Data: Tensile strength of Portland Cement

### a. Set up a data frame , with varibles: mixing (factor) and strength (response)

one <- c(3129, 3000, 2865, 2890)
two <- c(3200, 3300, 2975, 3150)
three <- c(2800, 2900, 2985, 3050)
four <- c(2600, 2700, 2600, 2765)
strength <- c(one, two, three, four)

material <- rep(c("1", "2", "3", "4"), each=4)
material <-factor(material)
cement  <-data.frame(cbind(strength, material))


### b. Perform a stripchart. Perform a Box plot.

stripchart(strength~material, vertical=TRUE, pch=16, main="Stripchart")

strength.means<-tapply(strength, material, mean)
lines(strength.means)

### c. Test the hypothesis that mixing techniques affect the strength of the cement. Use α=0.05
### What test do use. Perform the test. Conclusion.
# H0 = mixing techniques doesn't affect the strength of material
# Ha = mixing techniques affect the strength of material

#assumptions
# data is normal
#check normality
strength.aov.model<- aov(strength ~ material)
summary.aov(strength.aov.model)
summary.lm(strength.aov.model)


res <- residuals(strength.aov.model)
qqnorm(res, main="normality")
qqline(res)
shapiro.test(res)

#check variance
bartlett.test(res~material)
# pvalue is large  > alpha (0.05), reject null hypothesis, accept alternative
# material does affect the strength 



### d. Use the Fisher LSD (Least Significant Difference) 𝛼 = 0.05 to make comparison
#install.packages("agricolae")
library(agricolae)

MSerror <- 12826
Fisher<- LSD.test(strength.aov.model, "material", MSerror, console=T)
Fisher
## Q 3 
one <- c(143, 141, 150, 146)
two <- c(152, 149, 137, 143)
three <- c(134, 136, 132, 127)
four <- c(129, 127, 132, 129)

conductivity <- c(one, two, three, four)

coating <- rep(c("1", "2", "3", "4"), each=4)
coating <-factor(coating)
television  <- data.frame(cbind(conductivity, coating))

### a. Is there a difference in conductivity due to coating type?
### alpha = 0.05

cond.model<- aov(conductivity ~ coating)
summary.aov(cond.model)
summary.lm(cond.model)

res <- residuals(cond.model)
qqnorm(res, main="normality")
qqline(res)
shapiro.test(res)
# pvalue is greater than 0.05,so it's ascertained that the data is normal


# compare the variance
# H0 = there is no difference in conductivity var1 = var2
# Ha = there is a difference in conductivity var1 <> var2

bartlett.test(res~coating)

# pvalue is greater than 0.05, so null hypothesis is accepted and it's ascertained 
# the variance is same

#Since the data is normal and variance is equal, it may be concluded that 
# there is no difference in conductivity due to coating type


### b. Estimate the mean and the treatment effects view the model output
stripchart(conductivity~coating, vertical=TRUE, pch=16, main="Stripchart")

cond.means<-tapply(conductivity, coating, mean)
lines(cond.means)
# it can be seen from the stripchart that coating type of 1,2 have the same conductivity
# while 3, 4 have another group of same conductivity, but doesn't match 1,2

#The pvalue from aov test is smaller than 0.05, so we have sufficient evidence to say
# that one of the means is different from the others.
coating
tukey.95 <- TukeyHSD(cond.model, "coating")
tukey.95

plot(tukey.95)
# for coating type 4, the confidence intervals for the mean value 
# between groups 4-2 and 4-1 contain the value zero, 
# which indicates that there is a statistically significant difference 
# in mean loss between the two groups. 
# This is consistent with the fact that two of these groups of the 
# p-values from our hypothesis tests are below 0.05.

tukey.99 <- TukeyHSD(cond.model, "coating", conf.level = 0.99)
tukey.99

plot(tukey.99)
# for coating type 4, the confidence intervals for the mean value 
# between groups 4-2 and 4-1 contain the value zero, 
# which indicates that there is a statistically significant difference 
# in mean loss between the two groups. 
# This is consistent with the fact that two of these groups of the 
# p-values from our hypothesis tests are below 0.05.

# There is not much statistical difference between .95 and .99 


MSerror <-19.69 
Fisher<- LSD.test(cond.model, "coating", MSerror, console=T)
Fisher

plot(Fisher)