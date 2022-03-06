# 1 factor design
# power of a test: sample size

##Balance design; linear model
cpw<- c(7.66,6.98,7.80)
vp<-c(5.26,5.44,5.80)
comp <-c(7.41,7.33,7.04)
co2 <- c(3.51,2.91,3.66)

log_count <-c(cpw, vp, comp, co2)
package <- rep(c("1.cpw", "2.vp","3.comp", "4.co2"), each=3)
package<-as.factor(package)
#package <- rep(c(1, 2, 3, 4), each=3)
#relevel(package, ref="3")

bacteria <- data.frame(cbind(package, log_count))

attach(bacteria)

#means
means<-tapply(log_count, package, mean)
means

#std dev
sd<- tapply(log_count, package, sd)
sd

stripchart(log_count~package, vert=TRUE, pch=16, main="Stripchart")
lines(means, col="red")

boxplot(log_count~package, main="Boxplot log_count ~package")

##linear modelHTG6
bact.aov.model <-aov(log_count~package)
bact.aov.model
summary.aov(bact.aov.model)


#pvalue <alpha so reject null hypothesis and accept alternative hypothesis

summary.lm(bact.aov.model)

TukeyHSD(bact.aov.model)

#contrast means
C1 <- c(1,0,-1,0)

C2 <- c(0,1,-1/2,-1/2)
                                                                                                                                                                                                                                                                                                                                                                                                                                
C <- rbind(C1,C2)
rownames(C) <- c("Tr1 ==Tr3", "Tr2 = average of Tr3, Tr4")

library(gmodels)

fit.contrast(bact.aov.model, package, C, conf.int=0.95)

#C> fit.contrast(bact.aov.model, package, C, conf.int=0.95)
#Estimate Std. Error   t value  Pr(>|t|)   lower CI  upper CI
#packageTr1 ==Tr3                     0.22  0.2779089 0.7916264 0.4514097 -0.4208590 0.8608590
#packageTr2 = average of Tr3, Tr4     0.19  0.2406761 0.7894426 0.4526137 -0.3650002 0.7450002
#attr(,"class")

# see the pvalue to evaluate if mu1 == m2 or other scenarios in contrast
#if pvalue < alpha then reject null (mu1 == mu2) or that scenario

#C. power of a test
#b = type ii error of a test = p(accrpt h0|ho is false)
#power of test = 1-b