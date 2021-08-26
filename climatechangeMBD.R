rm(list=ls())

## Set your directory
dir="C:/Users/aa84249537/Desktop/Manuscripts/COVID and dengue/"
dir1="C:/Users/aa84249537/Desktop/Book Chapters and Reports/climatechange and vectorborne chapter/"

## load-in the necessary libraries
library(weathermetrics)
library(MMWRweek)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mgcv)

## read-in and manage temperature and precipitation data
dat<-read.csv(paste0(dir,"weekly-infectious-disease-bulletin-cases/2356391.csv"),header = T)
dat$DATE<-as.Date(dat$DATE)
dat$tave<-fahrenheit.to.celsius(dat$TAVG)

## precipitation is in inches
dat[,c("MMWRyear","MMWRweek","MMWRday")]<-MMWRweek(dat$DATE)

## cumulative precipitation and mean temperature
dat1<-as.data.frame(dat %>%
                      group_by(MMWRyear,MMWRweek) %>%
                      summarize(precip=sum(PRCP,na.rm=T),tave=mean(tave,na.rm = T)))
names(dat1)[c(1,2)]<-c("epi_year","epi_week")

## load dengue incidence data
dat2<-read.csv(paste0(dir,"weekly-infectious-disease-bulletin-cases/weekly-infectious-disease-bulletin-cases.csv"),header = T)
dat2<-dat2[dat2$disease=="Dengue Fever",]
dat2$epi_year<-substr(dat2$epi_week,1,4)
dat2$epi_week<-substr(dat2$epi_week,7,8)
dat2$epi_week<-as.numeric(as.character(dat2$epi_week))

## merge
dat3<-merge(dat1,dat2,by=c("epi_year","epi_week"),all.y=T)
dat3$yearwk<-paste0(dat3$epi_year,"-",dat3$epi_week)
dat3$time<-1:length(dat3$epi_year)
names(dat3)[6]<-"denguefever"

## use only before 2020
dat3<-dat3[dat3$epi_year<2020,]

## plot time-series for both dengue and average temperature
p1<-ggplot(dat3,aes(x=time,y=denguefever))+
  geom_point(size=3)+
  geom_line()+
  ylab("Weekly dengue incidence")+
  xlab("Time counter (in weeks)")+
  geom_vline(xintercept = c(which.max(dat3$denguefever)-5,which.max(dat3$denguefever)+5),lty=2,col="red")+
  theme_bw()


p2<-ggplot(dat3,aes(x=time,y=tave))+
  geom_point(size=3)+
  geom_line()+
  ylab("Weekly average temperature")+
  xlab("Time counter (in weeks)")+
  geom_vline(xintercept = c(which.max(dat3$denguefever)-5,which.max(dat3$denguefever)+5),lty=2,col="red")+
  theme_bw()

tiff(paste0(dir1,"Figure1.tiff"),height = 10,width = 20,units="in",pointsize = 12,compression = "lzw",
     type="cairo",res=300)
grid.arrange(p1,p2)
dev.off()


## Creating the E-R function
## We will utilize the generalized additive model in this case
## Simple specification: 
## 2df per year
## 4df for average temperature 

mod<-gam(denguefever~s(tave,k=4)+s(epi_week,k=16),family = quasipoisson(link="log"),data=dat3)
summary(mod)
plot(mod,select = 1) ## seems linear, we can opt for a linear function
p3<-plot(mod,select = 1)

tab1<-cbind.data.frame(x=p3[[1]]$x,fit=p3[[1]]$fit,se=p3[[1]]$se)
tab1$rr<-exp(tab1$fit)
tab1$cil<-exp(tab1$fit-1.96*tab1$se)
tab1$ciu<-exp(tab1$fit+1.96*tab1$se)

tiff(paste0(dir1,"Figure2.tiff"),height = 10,width = 10,units="in",pointsize = 12,compression = "lzw",
     type="cairo",res=300)
ggplot(tab1,aes(x=x,y=rr))+
  geom_point()+
  geom_errorbar(aes(ymin=cil,ymax=ciu))+
  xlab("Weekly average temperature (in degree Celsius)")+
  ylab("Relative risk")+
  theme_bw()+
  theme(text=element_text(size=30))
dev.off()


mod<-gam(denguefever~s(epi_week,k=16)+tave,family = quasipoisson(link="log"),data=dat3)
summary(mod) 
Epi::ci.lin(mod,subset = "tave",Exp=T) ## here we observe that with a 1 degree increase of temperature, there is approximately 24% 
## increase in dengue incidence


## Futures modeling
dat4<-read.csv("1-s2.0-S2589537020304569-mmc3.csv",header = T)

daly<-dat4[dat4$measure=="DALYs (Disability-Adjusted Life Years)" & dat4$metric=="Rate",]

## top 10 high dengue burden countries
val<-daly[,c("location","val","year")]
val<-val[val$year==2017,]
val[order(val$val,decreasing = T),][1:10,]$location


daly1<-daly[which(daly$location %in% val[order(val$val,decreasing = T),][1:10,]$location),]
names(daly1)[2]<-"Country"

tiff(paste0(dir1,"deng1.tiff"),height = 10,width = 20,units="in",pointsize = 12,compression = "lzw",
     type="cairo",res=300)
ggplot(daly1,aes(x=year,y=val,group=Country,col=Country))+
  geom_point(size=2)+
  geom_line()+
  xlab("Years")+
  ylab("Disability Life Years (DALY) per 100,000 population")+
  theme_bw()+
  theme(text = element_text(size=20))
dev.off()