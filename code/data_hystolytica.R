

#require(dplyr)
#require(plyr)
#library(rts)


#Create data frame to be use when creating the pomp object for histolytica 
#read data
J<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/disease_data_mexicoCity")

#Select type of pathogen
j=1#Amebiasis

#Select Delegation
i=9 #Iztapalapa
tss<-as.vector(J[i,j,c(4,5,8,1,9,7,6,2,12,11,10,3),1])
for(y in 2:7){
  tss<-c(tss,as.vector(J[i,j,c(4,5,8,1,9,7,6,2,12,11,10,3),y]))
}
df <- data.frame(date=seq(2005, 2012-(1/12),length.out = length(tss)), Amebiasis = tss)
dat_IZ=data.frame(time=round(df$date,2),Amebiasis=df$Amebiasis)

m_rec<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_iztapalapa_stations")

m_rec_IZ<-subset(m_rec,station_name==levels(m_rec$station_name)[1])

covartable_IZ <- data.frame(
  time=m_rec_IZ$time[which(m_rec_IZ$time>=2005 & m_rec_IZ$time<2012)],
  RR=m_rec_IZ$Mrain[which(m_rec_IZ$time>=2005 & m_rec_IZ$time<2012)]
)
covartable_IZ$RR<-as.numeric(as.character(covartable_IZ$RR))
max_rain_IZ=max(covartable_IZ$RR,na.rm=T)
###################################################################################
m_rec_XO<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_XOCHIMILCO_stations")
m_rec_XO<-subset(m_rec_XO,station_name==levels(m_rec_XO$station_name)[1])

covartable_XO <- data.frame(
  time=m_rec_XO$time[which(m_rec_XO$time>=2005 & m_rec_XO$time<2012)],
  RR=m_rec_XO$Mrain[which(m_rec_XO$time>=2005 & m_rec_XO$time<2012)]
)
covartable_XO$RR<-as.numeric(as.character(covartable_XO$RR))
max_rain_XO=max(covartable_XO$RR,na.rm=T)

######################################################################################
###################################################################################
m_rec_GM<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_GAMadero_stations")
m_rec_GM<-subset(m_rec_GM,station_name==levels(m_rec_GM$station_name)[1])

covartable_GM <- data.frame(
  time=m_rec_GM$time[which(m_rec_GM$time>=2005 & m_rec_GM$time<2012)],
  RR=m_rec_GM$Mrain[which(m_rec_GM$time>=2005 & m_rec_GM$time<2012)]
)
covartable_GM$RR<-as.numeric(as.character(covartable_GM$RR))
max_rain_GM=max(covartable_GM$RR,na.rm=T)

######################################################################################
###################################################################################
m_rec_AO<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_AOBREGON_stations")
m_rec_AO<-subset(m_rec_AO,station_name==levels(m_rec_AO$station_name)[1])

covartable_AO <- data.frame(
  time=m_rec_AO$time[which(m_rec_AO$time>=2005 & m_rec_AO$time<2012)],
  RR=m_rec_AO$Mrain[which(m_rec_AO$time>=2005 & m_rec_AO$time<2012)]
)
covartable_AO$RR<-as.numeric(as.character(covartable_AO$RR))
max_rain_AO=max(covartable_AO$RR,na.rm=T)
######################################################################################


###################################################################################
m_rec_AZ<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_AZCAPOTZALCO_stations")
m_rec_AZ<-subset(m_rec_AZ,station_name==levels(m_rec_AZ$station_name)[1])

covartable_AZ <- data.frame(
  time=m_rec_AZ$time[which(m_rec_AZ$time>=2005 & m_rec_AZ$time<2012)],
  RR=m_rec_AZ$Mrain[which(m_rec_AZ$time>=2005 & m_rec_AZ$time<2012)]
)
covartable_AZ$RR<-as.numeric(as.character(covartable_AZ$RR))
max_rain_AZ=max(covartable_AZ$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_BJ<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_BJUAREZ_stations")
m_rec_BJ<-subset(m_rec_BJ,station_name==levels(m_rec_BJ$station_name)[1])

covartable_BJ <- data.frame(
  time=m_rec_BJ$time[which(m_rec_BJ$time>=2005 & m_rec_BJ$time<2012)],
  RR=m_rec_BJ$Mrain[which(m_rec_BJ$time>=2005 & m_rec_BJ$time<2012)]
)
covartable_BJ$RR<-as.numeric(as.character(covartable_BJ$RR))
max_rain_BJ=max(covartable_BJ$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_CO<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_COYOACAN_stations")
m_rec_CO<-subset(m_rec_CO,station_name==levels(m_rec_CO$station_name)[1])

covartable_CO <- data.frame(
  time=m_rec_CO$time[which(m_rec_CO$time>=2005 & m_rec_CO$time<2012)],
  RR=m_rec_CO$Mrain[which(m_rec_CO$time>=2005 & m_rec_CO$time<2012)]
)
covartable_CO$RR<-as.numeric(as.character(covartable_CO$RR))
max_rain_CO=max(covartable_CO$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_CU<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_CUAJIMALPA_stations")
m_rec_CU<-subset(m_rec_CU,station_name==levels(m_rec_CU$station_name)[1])

covartable_CU <- data.frame(
  time=m_rec_CU$time[which(m_rec_CU$time>=2005 & m_rec_CU$time<2012)],
  RR=m_rec_CU$Mrain[which(m_rec_CU$time>=2005 & m_rec_CU$time<2012)]
)
covartable_CU$RR<-as.numeric(as.character(covartable_CU$RR))
max_rain_CU=max(covartable_CU$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_CUA<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_CUAUHTEMOC_stations")
m_rec_CUA<-subset(m_rec_CUA,station_name==levels(m_rec_CUA$station_name)[1])

covartable_CUA <- data.frame(
  time=m_rec_CUA$time[which(m_rec_CUA$time>=2005 & m_rec_CUA$time<2012)],
  RR=m_rec_CUA$Mrain[which(m_rec_CUA$time>=2005 & m_rec_CUA$time<2012)]
)
covartable_CUA$RR<-as.numeric(as.character(covartable_CUA$RR))
max_rain_CUA=max(covartable_CUA$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_IZTC<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_IZTACALCO_stations")
m_rec_IZTC<-subset(m_rec_IZTC,station_name==levels(m_rec_IZTC$station_name)[1])

covartable_IZTC<- data.frame(
  time=m_rec_IZTC$time[which(m_rec_IZTC$time>=2005 & m_rec_IZTC$time<2012)],
  RR=m_rec_IZTC$Mrain[which(m_rec_IZTC$time>=2005 & m_rec_IZTC$time<2012)]
)
covartable_IZTC$RR<-as.numeric(as.character(covartable_IZTC$RR))
max_rain_IZTC=max(covartable_IZTC$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_MI<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_MHIDALGO_stations")
m_rec_MI<-subset(m_rec_MI,station_name==levels(m_rec_MI$station_name)[1])

covartable_MI <- data.frame(
  time=m_rec_MI$time[which(m_rec_MI$time>=2005 & m_rec_MI$time<2012)],
  RR=m_rec_MI$Mrain[which(m_rec_MI$time>=2005 & m_rec_MI$time<2012)]
)
covartable_MI$RR<-as.numeric(as.character(covartable_MI$RR))
max_rain_MI=max(covartable_MI$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_MC<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_MCONTRERAS_stations")
m_rec_MC<-subset(m_rec_MC,station_name==levels(m_rec_MC$station_name)[1])

covartable_MC <- data.frame(
  time=m_rec_MC$time[which(m_rec_MC$time>=2005 & m_rec_MC$time<2012)],
  RR=m_rec_MC$Mrain[which(m_rec_MC$time>=2005 & m_rec_MC$time<2012)]
)
covartable_MC$RR<-as.numeric(as.character(covartable_MC$RR))
max_rain_MC=max(covartable_MC$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_MA<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_MILPAALTA_stations")
m_rec_MA<-subset(m_rec_MA,station_name==levels(m_rec_MA$station_name)[1])

covartable_MA <- data.frame(
  time=m_rec_MA$time[which(m_rec_MA$time>=2005 & m_rec_MA$time<2012)],
  RR=m_rec_MA$Mrain[which(m_rec_MA$time>=2005 & m_rec_MA$time<2012)]
)
covartable_MA$RR<-as.numeric(as.character(covartable_MA$RR))
max_rain_MA=max(covartable_MA$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_TA<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_TLAHUAC_stations")
m_rec_TA<-subset(m_rec_TA,station_name==levels(m_rec_TA$station_name)[1])

covartable_TA<- data.frame(
  time=m_rec_TA$time[which(m_rec_TA$time>=2005 & m_rec_TA$time<2012)],
  RR=m_rec_TA$Mrain[which(m_rec_TA$time>=2005 & m_rec_TA$time<2012)]
)
covartable_TA$RR<-as.numeric(as.character(covartable_TA$RR))
max_rain_TA=max(covartable_TA$RR,na.rm=T)
######################################################################################
###################################################################################
m_rec_TLP<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_TLALPAN_stations")
m_rec_TLP<-subset(m_rec_TLP,station_name==levels(m_rec_TLP$station_name)[1])

covartable_TLP<- data.frame(
  time=m_rec_TLP$time[which(m_rec_TLP$time>=2005 & m_rec_TLP$time<2012)],
  RR=m_rec_TLP$Mrain[which(m_rec_TLP$time>=2005 & m_rec_TLP$time<2012)]
)
covartable_TLP$RR<-as.numeric(as.character(covartable_TLP$RR))
max_rain_TLP=max(covartable_TLP$RR,na.rm=T)

######################################################################################
###################################################################################
m_rec_VC<-readRDS("c:/Users/abaezaca/Documents/GitHub/Histolytica/data/monthy_rainfall_VCARRANZA_stations")
m_rec_VC<-subset(m_rec_VC,station_name==levels(m_rec_VC$station_name)[1])

covartable_VC<- data.frame(
  time=m_rec_VC$time[which(m_rec_VC$time>=2005 & m_rec_VC$time<2012)],
  RR=m_rec_VC$Mrain[which(m_rec_VC$time>=2005 & m_rec_VC$time<2012)]
)
covartable_VC$RR<-as.numeric(as.character(covartable_VC$RR))
max_rain_VC=max(covartable_VC$RR,na.rm=T)
######################################################################################






png(filename = "hystolitica_casesRainfall_Iz.png",width = 18,height = 14,units = "cm",res = 200)
par(mar=c(4,4,2,4)+1)
plot(dat_IZ$time,dat_IZ$Amebiasis,type="o",ylim=c(0,400),col="red",pch=16,xlab="Time [years]",ylab="Cases")
axis(2,col="red",col.axis="red")
par(new=T)
plot(covartable_IZ$time,covartable_IZ$RR,type="o",axes = FALSE, bty = "n", xlab = "", ylab = "",col="blue",pch=16)
axis(side = 4,col="blue",col.axis="blue")
mtext(side = 4, line = 3, 'Rainfall',col="blue",col.axis="blue",las=3)
legend("topleft",legend=c("cases","rainfall"),
       text.col=c("red","blue"),pch=c(16,16),col=c("red","blue"))
dev.off()

