
type.data <- rep(c(1,2,3), each=12)
temp.data <- rep(c(15,70,125), each=4, time=3)

life <- c(130,155,74,180,34,40,80,75,20,70,82,58,150,188,159,126,136,122,106,
      115,25,70,58,45,138,110,168,160,174,120,150,139,96,104,82,60)
battery <- data.frame(type.data, temp.data, life)
battery$type <- factor(type.data, levels = c(1,2,3), 
                   labels = c("Type1", "Type2", "Type3"))
battery$temp <- factor(temp.data, levels = c(15,70,125), 
                   labels = c("Temp15", "Temp70", "Temp125"))

str(battery)

interaction.plot(type, temp, life, data=battery)
boxplot(life~type+temp)

battery.mod <-aov(life~type*temp, data=battery)
summary.aov(battery.mod)

#determine which combination gives the best battery life

model.tables(battery.mod, type="mean")
#type2 and temp15 gives the best battery life

########################################################################


library(daewr)
str(COdata)
head(COdata)

boxplot(CO~Eth*Ratio, data = COdata)
interaction.plot(COdata$Eth, COdata$Ratio, COdata$CO)
interaction.plot( COdata$Ratio, COdata$Eth, COdata$CO)

CO.mod <-aov(CO~Eth*Ratio, data=COdata)
summary(CO.mod)

model.tables(CO.mod, type = "means", se=T)

attach(COdata)
library(rsm) 
Eth.num<- as.numeric(Eth) #Eth.num is numeric 
Ratio.num <- as.numeric(Ratio) #Ratio.num is numeric 
COdata.new<-data.frame(Eth.num, Ratio.num, CO) 
CO.rsm<- rsm(CO ~ SO(Eth.num, Ratio.num), data=COdata.new) 
summary(CO.rsm)

canonical(CO.rsm)

xs <-canonical(CO.rsm)$xs
contour(CO.rsm,~Eth.num+Ratio.num, at=xs, image=TRUE)


