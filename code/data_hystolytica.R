

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

max_rain=max(covartable_IZ$RR,na.rm=T)
plot(dat_IZ$time,dat_IZ$Amebiasis,type="l",ylim=c(0,400))
lines(covartable_IZ$time,covartable_IZ$RR)


