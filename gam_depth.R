setwd("C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry")
library(mgcv)
library(visreg)
library(plyr)
library(ggplot2)
library(gridExtra)
library(splines)
library(gam)

cd <- read.csv("gbr4_data.csv")
cd <- cd[complete.cases(cd),] # remove rows that have missing values (NA) 
all_data <- do.call(data.frame,lapply(cd, function(x) replace(x, is.infinite(x),NA)))  #replace infinite numbers with NA
all_data <- all_data[complete.cases(all_data),]  # remove rows that have missing values (NA)
days=as.numeric(strftime(all_data$date, '%j'))-1 #transforms date to days
all_data <- cbind(all_data,days)
  labs(x=expression(DIN~(mg~N~m^-3)),  y="")

gam <- lm(Tricho_N ~ ns(DIN,df=5)+ns(temperature,df=5)+ns(days,df=3)+ns(salt,df=4) + ns(depth,df=4) + ns(DIP,df=5) + ns(PAR, df=5), data=all_data)


require(visreg)
require(plyr)
plotdata <- visreg(gam, type = 'contrast', plot = FALSE)

fitdata = plotdata[[1]]$fit  #get fitted data
resdata = plotdata[[1]]$res  #get residual data
x1 = fitdata[,1]
x2 = resdata[,1]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p1= ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12),
        panel.border = element_rect(colour='black', fill=NA)) +
  labs(x=expression(DIN~(mg~N~m^-3)),  y="")


fitdata = plotdata[[2]]$fit  #get fitted data
resdata = plotdata[[2]]$res  #get residual data
x1 = fitdata[,2]
x2 = resdata[,2]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p2 = ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12), #axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour='black', fill=NA)) +
  xlim(17,32) +
  ylim(-0.3,0.3) +
  labs(x='SST (°C)', y="")

fitdata = plotdata[[3]]$fit  #get fitted data
resdata = plotdata[[3]]$res  #get residual data
x1 = fitdata[,3]
x2 = resdata[,3]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p3 = ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12), #axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour='black', fill=NA)) +
  labs(x='Day of year', y="", colour = "")

fitdata = plotdata[[4]]$fit  #get fitted data
resdata = plotdata[[4]]$res  #get residual data
x1 = fitdata[,4]
x2 = resdata[,4]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p4 = ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12),
        panel.border = element_rect(colour='black', fill=NA)) +
  xlim(30,37) +
  labs(x='Salinity (PSU)', y=expression(italic(Trichodesmium)~N~(mg~N~m^-3)), colour = "")


fitdata = plotdata[[5]]$fit  #get fitted data
resdata = plotdata[[5]]$res  #get residual data
x1 = fitdata[,5]
x2 = resdata[,5]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p5 = ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12), #axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour='black', fill=NA)) +
  ylim(-0.2,0.5) +
  labs(x="Depth (m)", y="", colour = "")




fitdata = plotdata[[6]]$fit  #get fitted data
resdata = plotdata[[6]]$res  #get residual data
x1 = fitdata[,6]
x2 = resdata[,6]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p6 = ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12), #axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour='black', fill=NA)) +
  labs(x=expression(DIP~(mg~P~m^-3)), y="")



fitdata = plotdata[[7]]$fit  #get fitted data
resdata = plotdata[[7]]$res  #get residual data
x1 = fitdata[,7]
x2 = resdata[,7]
y1 = fitdata$visregFit
y2 = resdata$visregRes
p7 = ggplot(fitdata, aes_string(x1, y1)) + 
  geom_point(resdata, mapping=aes_string(x2,y2),col="gray63", size=0.5) +  
  geom_line(col="cornflowerblue", size=1.5) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.2,) +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#E6E6E3"), 
        panel.grid.minor = element_line(colour = '#E6E6E3'), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=12), #axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour='black', fill=NA)) +
  labs(x= expression(PAR~(mol~photon~m^-2~s^-1)), y="")


plt_data= list(p1,p2,p3,p4,p5,p6,p7)
arg_list <- c(plt_data, list(nrow=3, ncol=3))
do.call(grid.arrange, arg_list)



