pacman::p_load(dplyr,ggplot2,mgcv,ggpubr,gridExtra,data.table,here,RColorBrewer,tidyverse)

#Seasonal: Means, Thermal Shock Mins, Temp Ranges, F-T Cycles, Surface-Fracture Gradient (Mean, Range, and 24-hour curve)
#Overall: Mean, Temp Range, Thermal Shock Mins, F-T Cycles, Surface-Fracture Gradient (Mean, Range, and 24 hour loop)

#Data Processing -----

#Loading .csv files
filenames <- list.files(here("Processed Data/Finalized Year-Round Data"),pattern = "\\.csv$",full.names=TRUE)
filenames <- filenames[-c(1,2,3,4)]

#Check length of each data file
ldf <- lapply(filenames, read.csv)
res <- lapply(ldf,nrow)
names(res) <- substr(filenames,83,100)

#Calculate summary statistics for each site, sorting by season
df_seasonal=data.frame()
seasonal_means=data.frame()
for(i in 1:length(filenames)) {                            
  data <- read.csv(filenames[i])
  data<-filter((data),Season!="")
  data$Grad<-(data$ST-data$FT) #Manually create new variable for surface-fracture temperature gradient
  seasonal_means <-  data %>%
    group_by(Season) %>%
    summarise(ST_Mean = mean(ST, na.rm = T),
              ST_Max = max(ST,na.rm = T),
              ST_Min = min(ST,na.rm = T),
              FT_Mean = mean(FT,na.rm = T),
              FT_Max = max(FT,na.rm = T),
              FT_Min = min(FT,na.rm = T),
              ST_1 = sum(ST_1,na.rm = T),
              ST_2 = sum(ST_2,na.rm = T),
              FT_1 = sum(FT_1,na.rm = T),
              FT_2 = sum(FT_2,na.rm = T),
              DT_Mean = mean(Grad,na.rm = T),
              DT_Max = max(Grad,na.rm = T),
              DT_Min = min(Grad,na.rm = T))
  seasonal_means$ST_Range = (seasonal_means$ST_Max - seasonal_means$ST_Min)
  seasonal_means$FT_Range = (seasonal_means$FT_Max - seasonal_means$FT_Min)
  seasonal_means$DT_Range = (as.numeric(seasonal_means$DT_Max) - as.numeric(seasonal_means$DT_Min))
  seasonal_means$Site = substr(filenames[i],83,100)
  seasonal_means$Site = gsub("\\..*","",seasonal_means$Site)
  df_seasonal= rbind(df_seasonal,seasonal_means)}

#Calculate summary statistics for each site, sorting by month
df_monthly=data.frame()
monthly_means=data.frame()
for(i in 1:length(filenames)) {                            
  data <- read.csv(filenames[i])
  data<-filter((data),Month!="")
  data$Grad<-(data$ST-data$FT)
  monthly_means <-  data %>%
    group_by(Month) %>%
    summarise(ST_Mean = mean(ST, na.rm = T),
              ST_Max = max(ST,na.rm = T),
              ST_Min = min(ST,na.rm = T),
              FT_Mean = mean(FT,na.rm = T),
              FT_Max = max(FT,na.rm = T),
              FT_Min = min(FT,na.rm = T),
              ST_1 = sum(ST_1,na.rm = T),
              ST_2 = sum(ST_2,na.rm = T),
              FT_1 = sum(FT_1,na.rm = T),
              FT_2 = sum(FT_2,na.rm = T),
              DT_Mean = mean(Grad,na.rm = T),
              DT_Max = max(Grad,na.rm = T),
              DT_Min = min(Grad,na.rm = T))
  monthly_means$ST_Range = (monthly_means$ST_Max - monthly_means$ST_Min)
  monthly_means$FT_Range = (monthly_means$FT_Max - monthly_means$FT_Min)
  monthly_means$DT_Range = (as.numeric(monthly_means$DT_Max) - as.numeric(monthly_means$DT_Min))
  monthly_means$Site = substr(filenames[i],83,100)
  monthly_means$Site = gsub("\\..*","",monthly_means$Site)
  df_monthly= rbind(df_monthly,monthly_means)}

#Calculate summary statistics for each site, sorting by hour
df_hourly=data.frame()
for(i in 1:length(filenames)) {                            
  data <- read.csv(filenames[i])
  data<-filter((data),Hour!="")
  data$Grad<-(data$ST-data$FT)
  hourly_means <-  data %>%
    group_by(Hour) %>%
    summarise(ST_Mean = mean(ST, na.rm = T),
              ST_Max = max(ST,na.rm = T),
              ST_Min = min(ST,na.rm = T),
              FT_Mean = mean(FT,na.rm = T),
              FT_Max = max(FT,na.rm = T),
              FT_Min = min(FT,na.rm = T),
              ST_1 = sum(ST_1,na.rm = T),
              ST_2 = sum(ST_2,na.rm = T),
              FT_1 = sum(FT_1,na.rm = T),
              FT_2 = sum(FT_2,na.rm = T),
              DT_Mean = mean(Grad,na.rm = T),
              DT_Max = max(Grad,na.rm = T),
              DT_Min = min(Grad,na.rm = T))
  hourly_means$ST_Range = (hourly_means$ST_Max - hourly_means$ST_Min)
  hourly_means$FT_Range = (hourly_means$FT_Max - hourly_means$FT_Min)
  hourly_means$DT_Range = (as.numeric(hourly_means$DT_Max) - as.numeric(hourly_means$DT_Min))
  hourly_means$Site = substr(filenames[i],83,100)
  hourly_means$Site = gsub("\\..*","",hourly_means$Site)
  df_hourly= rbind(df_hourly,hourly_means)
}

#Calculate summary statistics for each site, sorting by hour and season
df_hourly_sorted=data.frame()
for(i in 1:length(filenames)) {                            
  data <- read.csv(filenames[i])
  data<-filter((data),Hour!="")
  data<-filter((data),Season!='')
  data$Grad<-(data$ST-data$FT)
  hourly_sorted <-  data %>%
    group_by(Season,Hour) %>%
    summarise(ST_Mean = mean(ST, na.rm = T),
              ST_Max = max(ST,na.rm = T),
              ST_Min = min(ST,na.rm = T),
              FT_Mean = mean(FT,na.rm = T),
              FT_Max = max(FT,na.rm = T),
              FT_Min = min(FT,na.rm = T),
              ST_1 = sum(ST_1,na.rm = T),
              ST_2 = sum(ST_2,na.rm = T),
              FT_1 = sum(FT_1,na.rm = T),
              FT_2 = sum(FT_2,na.rm = T),
              DT_Mean = mean(Grad,na.rm = T),
              DT_Max = max(Grad,na.rm = T),
              DT_Min = min(Grad,na.rm = T))
  hourly_sorted$ST_Range = (hourly_sorted$ST_Max - hourly_sorted$ST_Min)
  hourly_sorted$FT_Range = (hourly_sorted$FT_Max - hourly_sorted$FT_Min)
  hourly_sorted$DT_Range = (as.numeric(hourly_sorted$DT_Max)-as.numeric(hourly_sorted$DT_Min))
  hourly_sorted$Site = substr(filenames[i],83,100)
  hourly_sorted$Site = gsub("\\..*","",hourly_sorted$Site)
  df_hourly_sorted= rbind(df_hourly_sorted,hourly_sorted)
}

df_hourly_sorted$alpha <- rep(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                                0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                                0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0), 
                              length.out = nrow(df_hourly_sorted)) #Add alpha values to code for use in hysteresis plots

#Filter hourly data into individual sites
Hourly_Sydenham1<-filter((df_hourly_sorted),Site=="Sydenham1")
Hourly_Sydenham2<-filter((df_hourly_sorted),Site=="Sydenham2")
Hourly_Mountview5<-filter((df_hourly_sorted),Site=="Mountview5")
Hourly_Mountview6<-filter((df_hourly_sorted),Site=="Mountview6")
Hourly_Queen3<-filter((df_hourly_sorted),Site=="Queen3")
Hourly_Queen4<-filter((df_hourly_sorted),Site=="Queen4")

#Combine dataframes for each site into a list with names
hourly_list<-list(Hourly_Sydenham1,Hourly_Sydenham2,Hourly_Queen3,Hourly_Queen4,Hourly_Mountview5,Hourly_Mountview6)
names(hourly_list)<-c("Sydenham1","Sydenham2",'Queen3','Queen4','Mountview5','Mountview6')

#Seasonal Plots -----

#Surface Temperature Range and Fracture Temperature Range by Season
barplot2<-ggplot(data=df_seasonal, aes(x = Season, y = ST_Range,fill=Site)) + geom_bar(position='dodge',stat='identity')+
  labs(y="Surface Temperature Range (°C)", x="Season")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position="none",
                        legend.text=element_text(size=14),legend.title=element_text(size=15))+
  scale_fill_brewer(palette = "Paired")
barplot3<-ggplot(data=df_seasonal, aes(x = Season, y = FT_Range,fill=Site)) + geom_bar(position='dodge',stat='identity')+
  labs(y='Fracture Temperature Range (°C)', x="Season")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust=-0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust=+1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette = "Paired")
seasonal_ranges<-ggarrange(barplot2,barplot3,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Barplot1_TempRange.png"))

#Thermal Shock Minutes by Season
barplot4<-ggplot(data=df_seasonal, aes(x = Season, y = ST_1,fill=Site)) + geom_bar(position='dodge',stat='identity')+
  labs(y="Number of Minutes with dT>1°C at Surface", x="Season")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position="none",
                        legend.text=element_text(size=14),legend.title=element_text(size=15))+
  scale_fill_brewer(palette = "Paired")
barplot5<-ggplot(data=df_seasonal, aes(x = Season, y = FT_1,fill=Site)) + geom_bar(position='dodge',stat='identity')+
  labs(y='Number of Minutes with dT>1°C at Fracture', x="Season")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust=-0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust=+1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette = "Paired")
seasonal_ranges<-ggarrange(barplot4,barplot5,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Barplot2_ThermalShock.png"))

#Mean and Range of Surface-Fracture Temperature Gradient by Season
barplot6<-ggplot(data=df_seasonal, aes(x = Season, y = DT_Mean,fill=Site)) + geom_bar(position='dodge',stat='identity')+
  labs(y="Mean Surface-Fracture Temperature Gradient (°C)", x="Season")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position="none",
                        legend.text=element_text(size=14),legend.title=element_text(size=15))+
  scale_fill_brewer(palette = "Paired")
barplot7<-ggplot(data=df_seasonal, aes(x = Season, y = DT_Range,fill=Site)) + geom_bar(position='dodge',stat='identity')+
  labs(y='Range in Surface-Fracture Temperature Gradient (°C)', x="Season")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust=-0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust=+1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette = "Paired")
seasonal_DT<-ggarrange(barplot6,barplot7,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Barplot3_DT.png"))

#Monthly Plots -----

#Surface and Fracture Temperature Range by Month
lineplot1<-ggplot() + scale_x_continuous(limits = c(1,12))+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6, xmax = 9,
           ymin = -Inf, ymax = Inf) +
  geom_line(data=df_monthly, aes(x = Month, y = ST_Range,color=Site))+
  labs(y="Surface Temperature Range (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position="none",
                        legend.text=element_text(size=14),legend.title=element_text(size=15))+
  scale_color_brewer(palette = "Paired")
lineplot2<-ggplot() +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6, xmax = 9,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+
  geom_line(data=df_monthly, aes(x = Month, y = FT_Range,color=Site))+
  labs(y="Fracture Temperature Range (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
monthly_ranges<-ggarrange(lineplot1,lineplot2,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot1_TempRanges.png"))

#Loess-smoothed Surface and Fracture Temperature Range by Month
lineplot1b<-ggplot(data=df_monthly,aes(x=Month,y=FT_Range,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf)+
  labs(y="Surface Temperature Range (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot2b<-ggplot(data=df_monthly,aes(x=Month,y=FT_Range,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+
  labs(y="Fracture Temperature Range (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
monthly_TempRange_loess<-ggarrange(lineplot1b,lineplot2b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot1_TempRange_Loess.png"))

#Thermal Shock Minutes at Surface and Fracture by Month
lineplot3<-ggplot() + scale_x_continuous(limits = c(1,12))+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  geom_line(data=df_monthly, aes(x = Month, y = ST_1,color=Site))+
  labs(y="Number of Minutes meeting 1°C/min Threshold at Surface", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position="none",
                        legend.text=element_text(size=14),legend.title=element_text(size=15))+
  scale_color_brewer(palette = "Paired")
lineplot4<-ggplot() +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+
  geom_line(data=df_monthly, aes(x = Month, y = FT_1,color=Site))+
  labs(y="Number of Minutes meeting 1°C/min Threshold in Fracture", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
monthly_shock<-ggarrange(lineplot3,lineplot4,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot2_ThermalShock.png"))

#Loess-Smoothed Thermal Shock Minutes at Surface and Fracture by Month
lineplot3_b<-ggplot(data=df_monthly,aes(x=Month,y=ST_1,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf)+
  labs(y="Number of Minutes Meeting 1°C/Min Threshold at Surface", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot4_b<-ggplot(data=df_monthly,aes(x=Month,y=FT_1,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+
  labs(y="Number of Minutes Meeting 1°C/min Threshold in Fracture", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
monthly_Shock_loess<-ggarrange(lineplot3_b,lineplot4_b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot2_MonthlyShock_Loess.png"))

#Mean and Range of Surface-Fracture Temperature Gradient by Month
lineplot3a<-ggplot()+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+geom_line(data=df_monthly,aes(x=Month,y=DT_Mean,color=Site))+
  labs(y="Mean Surface-Fracture Temperature Gradient (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_color_brewer(palette = "Paired")
lineplot4a<-ggplot()+geom_line(data=df_monthly,aes(x=Month,y=DT_Range,color=Site))+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+
  labs(y="Range of Surface-Fracture Temperature Gradient (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
monthly_DT<-ggarrange(lineplot3a,lineplot4a,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot_MonthlyDT.png"))

#Loess-Smoothed Mean and Range of Surface-Fracture Temperature Gradient by Month
lineplot3b<-ggplot(data=df_monthly,aes(x=Month,y=DT_Mean,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf)+
  labs(y="Mean Surface-Fracture Temperature Gradient (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot4b<-ggplot(data=df_monthly,aes(x=Month,y=DT_Range,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 1, xmax = 3.5,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf) +
  xlim(1,12)+
  labs(y="Range of Surface-Fracture Temperature Gradient (°C)", x="Month")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
monthly_DT_loess<-ggarrange(lineplot3b,lineplot4b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot_MonthlyDT_Loess.png"))

#Hourly Plots -----

#Range of Surface and Fracture Temperature by Hour and Site
lineplot5<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=ST_Range,color=Site))+
  labs(y="Surface Temperature Range (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_color_brewer(palette = "Paired")
lineplot6<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=FT_Range,color=Site))+
  labs(y="Fracture Temperature Range (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
hourly_range<-ggarrange(lineplot5,lineplot6,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot3_HourlyRange.png"))

#Loess-smoothed Range of Surface and Fracture Temperature by Hour and Site
lineplot5b<-ggplot(data=df_hourly,aes(x=Hour,y=ST_Range,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Surface Temperature Range (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot6b<-ggplot(data=df_hourly,aes(x=Hour,y=FT_Range,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Range of Fracture Temperature (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
hourly_TempRange_loess<-ggarrange(lineplot5b,lineplot6b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot3b_HourlyRange_Loess.png"))

#Mean Surface and Fracture Temperature by Hour and Site
lineplot7<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=ST_Mean,color=Site))+
  labs(y="Mean Surface Temperature (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_color_brewer(palette = "Paired")
lineplot8<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=FT_Mean,color=Site))+
  labs(y="Mean Fracture Temperature (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
hourly_mean<-ggarrange(lineplot7,lineplot8,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot4_HourlyMean.png"))

#Loess-Smoothed Mean Surface and Fracture Temperature by Hour and Site
lineplot7b<-ggplot(data=df_hourly,aes(x=Hour,y=ST_Mean,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Mean Surface Temperature (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot8b<-ggplot(data=df_hourly,aes(x=Hour,y=FT_Mean,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Mean Fracture Temperature (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
hourly_TempMean_loess<-ggarrange(lineplot7b,lineplot8b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot4b_HourlyMeanTemp_Loess.png"))

#Thermal Shock Minutes at Surface and Fracture by Hour and Site
lineplot9<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=ST_1,color=Site))+
  labs(y='Number of Minutes Meeting 1°C/min Threshold at Surface', x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_color_brewer(palette = "Paired")
lineplot10<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=FT_1,color=Site))+
  labs(y='Number of Minutes Meeting 1°C/min Threshold in Fracture', x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
hourly_range<-ggarrange(lineplot9,lineplot10,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot5_HourlyShock.png"))

#Loess-Smoothed Thermal Shock Minutes at Surface and Fracture by Hour and Site
lineplot9b<-ggplot(data=df_hourly,aes(x=Hour,y=ST_1,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Number of Minutes Meeting 1°C/min Threshold at Surface", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot10b<-ggplot(data=df_hourly,aes(x=Hour,y=FT_1,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Number of Minutes Meeting 1°C/min Threshold in Fracture", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
hourly_Shock_loess<-ggarrange(lineplot9b,lineplot10b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot5_HourlyShock_Loess.png"))

#Mean and Range of Surface-Fracture Temperature Gradient by Hour and Site
lineplot11<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=DT_Mean,color=Site))+
  labs(y='Mean Surface-Fracture Temperature Gradient (°C)', x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_color_brewer(palette = "Paired")
lineplot12<-ggplot()+geom_line(data=df_hourly,aes(x=Hour,y=DT_Range,color=Site))+
  labs(y='Range in Surface-Fracture Temperature Gradient (°C)', x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_color_brewer(palette = "Paired")
hourly_DT<-ggarrange(lineplot11,lineplot12,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot6_HourlyDT.png"))

#Loess-Smoothed Mean and Range of Surface-Fracture Temperature Gradient by Hour and Site
lineplot11b<-ggplot(data=df_hourly,aes(x=Hour,y=DT_Mean,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Mean Surface-Fracture Temperature Gradient (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.position='none')+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
lineplot12b<-ggplot(data=df_hourly,aes(x=Hour,y=DT_Range,color=Site,fill=Site))+geom_smooth(method='loess',alpha=0.15)+
  labs(y="Range of Surface-Fracture Temperature Gradient (°C)", x="Hour")+
  theme_classic()+theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                        axis.title.x=element_text(size=20,face='bold',vjust = -0.75),  
                        axis.title.y=element_text(size=20,face='bold',vjust = +1.5),  
                        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(face = "bold",size=15),
                        legend.text=element_text(size=14),legend.background = element_blank(),
                        legend.box.background = element_rect(colour = "black"))+
  scale_fill_brewer(palette="Paired")+
  scale_color_brewer(palette = "Paired")
hourly_DT_loess<-ggarrange(lineplot11b,lineplot12b,ncol=2,nrow=1,labels='AUTO')
ggsave(filename = here("Figures","Lineplot6b_HourlyDT_Loess.png"))

#For loop to create figures for mean and range of surface-fracture temperature gradient by hour for each site individually, comparing different seasons
for (i in 1:length(hourly_list)){
  df_i <- hourly_list[[i]] #Sets dataframe as the i-th dataframe in the list
  fig <-  
    df_i %>%
    ggplot(aes(x=Hour,y=DT_Mean))+geom_line(aes(colour=Season))+scale_size_continuous(c(1,4))+ggtitle(paste0(names(hourly_list)[i]))+
    xlab("Hour")+ylab("Mean Surface-Fracture Temperature Gradient (°C)")+labs(colour="Season")+theme_classic()+
    theme(title = element_text(size=20,face='bold'),plot.title = element_text(hjust = 0.5),axis.title.x = element_text(size=20,face='bold'),axis.title.y = element_text(size=20,face='bold'),
          axis.text.x = element_text(size=17,colour='black'),axis.text.y = element_text(size=17,colour='black'),
          legend.title = element_text(face='bold'),legend.text = element_text(size=14),legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
  ggsave(fig,path=here("Figures"),filename=paste0(names(hourly_list)[i],"_DTMean.png"))
  fig2<-
    df_i %>%
    ggplot(aes(x=Hour,y=DT_Range))+geom_line(aes(colour=Season))+scale_size_continuous(c(1,4))+ggtitle(paste0(names(hourly_list)[i]))+
    xlab("Hour")+ylab("Range in Surface-Fracture Temperature Gradient (°C)")+labs(colour="Season")+theme_classic()+
    theme(title = element_text(size=20,face='bold'),plot.title = element_text(hjust = 0.5),axis.title.x = element_text(size=20,face='bold'),axis.title.y = element_text(size=20,face='bold'),
          axis.text.x = element_text(size=17,colour='black'),axis.text.y = element_text(size=17,colour='black'),
          legend.title = element_text(face='bold'),legend.text = element_text(size=14),legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
  ggsave(fig2,path=here("Figures"),filename=paste0(names(hourly_list)[i],"_DTRange.png"))
}

#For loop to create hysteresis figures for mean surface and fracture temperature by hour for each site individually, comparing different seasons
for (i in 1:length(hourly_list)){
  df_i <- hourly_list[[i]]
  fig <-  
    df_i %>%
    ggplot(aes(x=ST_Mean,y=FT_Mean))+geom_point(aes(size=Hour,colour=Season))+theme_classic()+
    geom_path(colour='black',alpha=df_i$alpha)+scale_size_continuous(c(1,4))+ggtitle(paste0(names(hourly_list)[i]))+
    xlab("Surface Temperature (°C)")+ylab("Fracture Temperature (°C)")+labs(colour="Probe",size="Hour of Day")+
    theme(title = element_text(size=20,face='bold'),plot.title = element_text(hjust = 0.5),axis.title.x = element_text(size=20,face='bold'),axis.title.y = element_text(size=20,face='bold'),
          axis.text.x = element_text(size=17,colour='black'),axis.text.y = element_text(size=17,colour='black'),
          legend.title = element_text(face='bold'),legend.text = element_text(size=14),legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
  ggsave(path=here("Figures"),filename=paste0(names(hourly_list)[i],"_Loop.png"))}