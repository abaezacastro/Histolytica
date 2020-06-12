

#Create data frame of covariateas for each monucipality 
#to be use when creating the pomp object for E. histolytica population dynamics
#
#read data
J<-readRDS("data/disease_data_mexicoCity")
#Select type of pathogen
#j=1#Amebiasis

#Select Delegation
#i=9 #Iztapalapa
#tss<-as.vector(J[i,j,c(4,5,8,1,9,7,6,2,12,11,10,3),1])
#for(y in 2:10){
#  tss<-c(tss,as.vector(J[i,j,c(4,5,8,1,9,7,6,2,12,11,10,3),y]))
#}
#df <- data.frame(date=seq(2005, 2012-(1/12),length.out = length(tss)), Amebiasis = tss)
#dat_IZ=data.frame(time=round(df$date,2),Amebiasis=df$Amebiasis)
load(file = "C:/Users/abaezaca/Dropbox (ASU)/MEGADAPT/rainfall/long_timeSeries_Stations")
m_rec<-readRDS("data/monthy_rainfall_iztapalapa_stations")

Rainfall_monthy<-data.frame(read.csv("data/rainfall/monthy_rainfall_2005_2011.csv",header = T))

m_rec_IZ<-subset(m_rec,station_name==levels(m_rec$station_name)[1])



covartable_IZ=subset(Rainfall_monthy,Municipality=="Iztapalapa")[,c(1,4)]

cov2<-data.frame(
time=seq.Date(as.Date("2001/1/1"),as.Date("2014/12/31"),by = 1),
RR=rain_station[[30]][[3]][-(1:31)]) # first part of the ts is 2000


dcov=subset(cov2,time>as.Date("2004/12/31") & time < as.Date("2012/1/1"))
dcov$time<-seq(from=2005,to =2012-1/length(dcov$time) ,length.out=length(dcov$time))
caovariable_IZ=list(covartable_IZ,zone=9,dcov)
######################################################################################
###################################################################################
m_rec_TLP<-readRDS("data/monthy_rainfall_TLALPAN_stations")
m_rec_TLP<-subset(m_rec_TLP,station_name==levels(m_rec_TLP$station_name)[1])

covartable_TLP<- data.frame(
  time=m_rec_TLP$time[which(m_rec_TLP$time>=2005 & m_rec_TLP$time<2012)],
  RR=m_rec_TLP$Mrain[which(m_rec_TLP$time>=2005 & m_rec_TLP$time<2012)]
)
covartable_TLP$RR<-as.numeric(as.character(covartable_TLP$RR))
max_rain_TLP=max(covartable_TLP$RR,na.rm=T)

covartable_TLP=subset(Rainfall_monthy,Municipality=="Tlalpan")[,c(1,4)]

cov2<-data.frame(
  time=seq.Date(as.Date("2001/1/1"),
                as.Date("2014/12/31"),
                by = 1),
  RR=rain_station[[37]][[3]][-(1:31)]) # first part of the ts is 2000
dcov=subset(cov2,time>as.Date("2004/12/31") & time < as.Date("2012/1/1"))
dcov$time<-seq(from=2005,to =2012-1/length(dcov$time) ,length.out=length(dcov$time))



caovariable_TLP=list(covartable_TLP,zone=14,dcov)
######################################################################################
###################################################################################
m_rec_BJ<-readRDS("data/monthy_rainfall_BJUAREZ_stations")
m_rec_BJ<-subset(m_rec_BJ,station_name==levels(m_rec_BJ$station_name)[1])

covartable_BJ <- data.frame(
  time=m_rec_BJ$time[which(m_rec_BJ$time>=2005 & m_rec_BJ$time<2012)],
  RR=m_rec_BJ$Mrain[which(m_rec_BJ$time>=2005 & m_rec_BJ$time<2012)]
)

covartable_BJ$RR<-as.numeric(as.character(covartable_BJ$RR))
max_rain_BJ=max(covartable_BJ$RR,na.rm=T)

covartable_BJ=subset(Rainfall_monthy,Municipality=="B_JUAREZ")[,c(1,4)]

cov2<-data.frame(
  time=seq.Date(as.Date("2001/1/1"),
                as.Date("2014/12/31"),
                by = 1),
  RR=rain_station[[19]][[3]][-(1:31)]) # first part of the ts is 2000
dcov=subset(cov2,time>as.Date("2004/12/31") & time < as.Date("2012/1/1"))
dcov$time<-seq(from=2005,to =2012-1/length(dcov$time) ,length.out=length(dcov$time))


caovariable_BJ=list(covartable_BJ,zone=3,dcov)
######################################################################################
###################################################################################
#m_rec_AO<-readRDS("data/monthy_rainfall_AOBREGON_stations")
#m_rec_AO<-subset(m_rec_AO,station_name==levels(m_rec_AO$station_name)[1])


covartable_AO=subset(Rainfall_monthy,Municipality=="A_OBREGON")[,c(1,4)]

cov2<-data.frame(
  time=seq.Date(as.Date("2001/1/1"),
                as.Date("2014/12/31"),
                by = 1),
  RR=rain_station[[17]][[3]][-(1:31)]) # first part of the ts is 2000
dcov=subset(cov2,time>as.Date("2004/12/31") & time < as.Date("2012/1/1"))
dcov$time<-seq(from=2005,to =2012-1/length(dcov$time) ,length.out=length(dcov$time))


caovariable_AO=list(covartable_AO,zone=1,dcov)
######################################################################################
###################################################################################
covartable_MA=subset(Rainfall_monthy,Municipality=="Milpa_Alta")[,c(1,4)]

cov2<-data.frame(
  time=seq.Date(as.Date("2001/1/1"),
                as.Date("2014/12/31"),
                by = 1),
  RR=rain_station[[43]][[3]][-(1:31)]) # first part of the ts is 2000
dcov=subset(cov2,time>as.Date("2004/12/31") & time < as.Date("2012/1/1"))
dcov$time<-seq(from=2005,to =2012-1/length(dcov$time) ,length.out=length(dcov$time))


caovariable_MA=list(covartable_MA,zone=12,dcov)

###################################################################################
m_rec_XO<-readRDS("data/monthy_rainfall_XOCHIMILCO_stations")
m_rec_XO<-subset(m_rec_XO,station_name==levels(m_rec_XO$station_name)[1])

covartable_XO <- data.frame(
  time=m_rec_XO$time[which(m_rec_XO$time>=2005 & m_rec_XO$time<2012)],
  RR=m_rec_XO$Mrain[which(m_rec_XO$time>=2005 & m_rec_XO$time<2012)]
)
covartable_XO$RR<-as.numeric(as.character(covartable_XO$RR))
max_rain_XO=max(covartable_XO$RR,na.rm=T)
caovariable_XO=list(covartable_XO,zone=16)
######################################################################################
###################################################################################
m_rec_GM<-readRDS("data/monthy_rainfall_GAMadero_stations")
m_rec_GM<-subset(m_rec_GM,station_name==levels(m_rec_GM$station_name)[1])

covartable_GM <- data.frame(
  time=m_rec_GM$time[which(m_rec_GM$time>=2005 & m_rec_GM$time<2012)],
  RR=m_rec_GM$Mrain[which(m_rec_GM$time>=2005 & m_rec_GM$time<2012)]
)
covartable_GM$RR<-as.numeric(as.character(covartable_GM$RR))
max_rain_GM=max(covartable_GM$RR,na.rm=T)
caovariable_GM=list(covartable_GM,zone=7)


###################################################################################
m_rec_AZ<-readRDS("data/monthy_rainfall_AZCAPOTZALCO_stations")
m_rec_AZ<-subset(m_rec_AZ,station_name==levels(m_rec_AZ$station_name)[1])

covartable_AZ <- data.frame(
  time=m_rec_AZ$time[which(m_rec_AZ$time>=2005 & m_rec_AZ$time<2012)],
  RR=m_rec_AZ$Mrain[which(m_rec_AZ$time>=2005 & m_rec_AZ$time<2012)]
)
covartable_AZ$RR<-as.numeric(as.character(covartable_AZ$RR))
max_rain_AZ=max(covartable_AZ$RR,na.rm=T)
caovariable_AZ=list(covartable_AZ,zone=2)
######################################################################################
###################################################################################
m_rec_CO<-readRDS("data/monthy_rainfall_COYOACAN_stations")
m_rec_CO<-subset(m_rec_CO,station_name==levels(m_rec_CO$station_name)[1])

covartable_CO <- data.frame(
  time=m_rec_CO$time[which(m_rec_CO$time>=2005 & m_rec_CO$time<2012)],
  RR=m_rec_CO$Mrain[which(m_rec_CO$time>=2005 & m_rec_CO$time<2012)]
)
covartable_CO$RR<-as.numeric(as.character(covartable_CO$RR))
max_rain_CO=max(covartable_CO$RR,na.rm=T)
caovariable_CO=list(covartable_CO,zone=4)
######################################################################################
###################################################################################
m_rec_CU<-readRDS("data/monthy_rainfall_CUAJIMALPA_stations")
m_rec_CU<-subset(m_rec_CU,station_name==levels(m_rec_CU$station_name)[1])

covartable_CU <- data.frame(
  time=m_rec_CU$time[which(m_rec_CU$time>=2005 & m_rec_CU$time<2012)],
  RR=m_rec_CU$Mrain[which(m_rec_CU$time>=2005 & m_rec_CU$time<2012)]
)
covartable_CU$RR<-as.numeric(as.character(covartable_CU$RR))
max_rain_CU=max(covartable_CU$RR,na.rm=T)
caovariable_CU=list(covartable_CU,zone=5)
######################################################################################
###################################################################################
m_rec_CUA<-readRDS("data/monthy_rainfall_CUAUHTEMOC_stations")
m_rec_CUA<-subset(m_rec_CUA,station_name==levels(m_rec_CUA$station_name)[1])

covartable_CUA <- data.frame(
  time=m_rec_CUA$time[which(m_rec_CUA$time>=2005 & m_rec_CUA$time<2012)],
  RR=m_rec_CUA$Mrain[which(m_rec_CUA$time>=2005 & m_rec_CUA$time<2012)]
)
covartable_CUA$RR<-as.numeric(as.character(covartable_CUA$RR))
max_rain_CUA=max(covartable_CUA$RR,na.rm=T)
caovariable_CUA=list(covartable_CUA,zone=6)
######################################################################################
###################################################################################
m_rec_IZTC<-readRDS("data/monthy_rainfall_IZTACALCO_stations")
m_rec_IZTC<-subset(m_rec_IZTC,station_name==levels(m_rec_IZTC$station_name)[1])

covartable_IZTC<- data.frame(
  time=m_rec_IZTC$time[which(m_rec_IZTC$time>=2005 & m_rec_IZTC$time<2012)],
  RR=m_rec_IZTC$Mrain[which(m_rec_IZTC$time>=2005 & m_rec_IZTC$time<2012)]
)
covartable_IZTC$RR<-as.numeric(as.character(covartable_IZTC$RR))
max_rain_IZTC=max(covartable_IZTC$RR,na.rm=T)
caovariable_IZTC=list(covartable_IZTC,zone=8)
######################################################################################
###################################################################################
m_rec_MI<-readRDS("data/monthy_rainfall_MHIDALGO_stations")
m_rec_MI<-subset(m_rec_MI,station_name==levels(m_rec_MI$station_name)[1])

covartable_MI <- data.frame(
  time=m_rec_MI$time[which(m_rec_MI$time>=2005 & m_rec_MI$time<2012)],
  RR=m_rec_MI$Mrain[which(m_rec_MI$time>=2005 & m_rec_MI$time<2012)]
)
covartable_MI$RR<-as.numeric(as.character(covartable_MI$RR))
max_rain_MI=max(covartable_MI$RR,na.rm=T)
caovariable_MI=list(covartable_MI,zone=11)
######################################################################################
###################################################################################
m_rec_MC<-readRDS("data/monthy_rainfall_MCONTRERAS_stations")
m_rec_MC<-subset(m_rec_MC,station_name==levels(m_rec_MC$station_name)[1])

covartable_MC <- data.frame(
  time=m_rec_MC$time[which(m_rec_MC$time>=2005 & m_rec_MC$time<2012)],
  RR=m_rec_MC$Mrain[which(m_rec_MC$time>=2005 & m_rec_MC$time<2012)]
)
covartable_MC$RR<-as.numeric(as.character(covartable_MC$RR))
max_rain_MC=max(covartable_MC$RR,na.rm=T)
caovariable_MC=list(covartable_MC,zone=10)
######################################################################################
###################################################################################
m_rec_TLH<-readRDS("data/monthy_rainfall_TLAHUAC_stations")
m_rec_TLH<-subset(m_rec_TLH,station_name==levels(m_rec_TLH$station_name)[1])

covartable_TLH<- data.frame(
  time=m_rec_TLH$time[which(m_rec_TLH$time>=2005 & m_rec_TLH$time<2012)],
  RR=m_rec_TLH$Mrain[which(m_rec_TLH$time>=2005 & m_rec_TLH$time<2012)]
)
covartable_TLH$RR<-as.numeric(as.character(covartable_TLH$RR))
max_rain_TLH=max(covartable_TLH$RR,na.rm=T)
caovariable_TLH=list(covartable_TLH,zone=13)
######################################################################################
###################################################################################
m_rec_VC<-readRDS("data/monthy_rainfall_VCARRANZA_stations")
m_rec_VC<-subset(m_rec_VC,station_name==levels(m_rec_VC$station_name)[1])

covartable_VC<- data.frame(
  time=m_rec_VC$time[which(m_rec_VC$time>=2005 & m_rec_VC$time<2012)],
  RR=m_rec_VC$Mrain[which(m_rec_VC$time>=2005 & m_rec_VC$time<2012)]
)
covartable_VC$RR<-as.numeric(as.character(covartable_VC$RR))
max_rain_VC=max(covartable_VC$RR,na.rm=T)
caovariable_VC=list(covartable_VC,zone=15)
######################################################################################

