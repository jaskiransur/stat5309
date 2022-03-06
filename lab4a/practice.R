type <- rep(c(1,2), each=12)
meth <- rep(c(1,2,3,4), each=3, times=2)
strength <- c(68,63,65,126,128,133,93,101,98,56,59,57,71,66,66,107,110,116,63,60,59,40,41,44)
length(strength)

concrete <- data.frame(type, meth, strength)
attach(concrete)

type <-factor(type, levels =1:2, labels=c("Basalt", "Silicious"))
meth <-factor(meth, levels =1:4, labels=c("Static","Reg", "Low","Very-Low"))

tapply(strength, list(type,meth), mean)
boxplot(strength~type, main="strength vs type")
boxplot(strength~meth, main="strength vs meth")
boxplot(strength~type*meth, main="strength vs type*meth")

interaction.plot(type, meth, strength)
interaction.plot(meth, type, strength)
interaction.plot(meth, type, strength)

# if the lines intersect then interaction effect is signficant

#linear model with interaction

concrete.mod <- aov(strength~type*meth)
summary.aov(concrete.mod)

summary.lm(concrete.mod)

#model without interaction to check if the interaction term should be used
concrete.nointeraction.mod <- aov(strength~type+meth)

#compare both the interactions
anova(concrete.mod, concrete.nointeraction.mod)
#if p values small then there is an interaction


model.tables(concrete.mod, type = "means")
model.tables(concrete.mod, type = "effects")

tukey <- TukeyHSD(concrete.mod)
plot(tukey)

LSD.test(concrete.mod)
