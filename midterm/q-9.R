Score<-c(23,24,25,36,35,36,28,24,27,27,28,26,34,38,39,35,35,34,31,32,29,33,34,
         35,26,27,25,24,23,28,37,39,35,26,29,25,38,36,35,34,38, 
         36,36,37,34,34,36,39,34,36,31,28,26,24)

Temperature.num<-c(300,300,300,300,300,300,300,300,300,300,300,300,
               300,300,300,300,300,300,300,300,300,300,300,300,
               300,300,300,350,350,350,350,350,350,350,350,350,
               350,350,350,350,350,350,350,350,350,350,350,350,
               350,350,350,350,350,350)

Operator.num<-c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,
                3,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3, 3,3,3,3) 

CycleTime.num<-c(40,40,40,50,50,50,60,60,60,40,40,40,50,50,50,60,60,60,40,40,40,
                 50,50,50,60,60,60,40,40,40,50,50,50,60,60,60,40,40,40,50,50,50,
                 60,60,60,40,40,40,50,50,50,60,60,60)
fabrics<-data.frame(Score,Temperature.num,Operator.num)
fabrics$Temperature<-as.factor(Temperature.num)
fabrics$Operator<-as.factor(Operator.num)
fabrics$CycleTime<-as.factor(CycleTime.num)

#par(mar=c(1, 1, 1, 1))
par(mfrow=c(2,2))
interaction.plot(Temperature,Operator,Score)
interaction.plot(CycleTime,Operator,Score)
interaction.plot(CycleTime,Temperature,Score)
