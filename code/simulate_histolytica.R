library(gridExtra)
library(magrittr)
require(ggplot2)
library(reshape2)
#simulate histolytica for Iztapalapa and plot time series
#################################################
sims <- simulate(histolytica_pomp_IZ,params=coef(miff2_Iztapalapa),
                 nsim=1,as.data.frame=TRUE,include.data=TRUE)

N_pop=fixed_params[2]
png(filename = "single_simualtion_example_Iztapalapa.png",width = 12,height = 8,units = "cm",res = 100)
plot.ts(dat_IZ$Amebiasis,ylab="cases Amebiasis")
  lines(sims$cases[85:168],col='red')
legend("topright",legend = c("data","simulation"),col = c("black","red"),lty=c(1,1))
dev.off()

plot(dat_IZ$Amebiasis[-1],sims$cases[86:168],ylim=c(0,300),xlim=c(0,300))
abline(1,1)
ggplot(sims,mapping=aes(y=Amebiasis,x=cases,color=sim))+
  geom_point(color="red")+
  scale_x_continuous("Observed",limits=c(0,400))+
  scale_y_continuous("Predicted",limits=c(0,400))+
  guides(color=FALSE)


################################################################################

trajectory_histolytica<-trajectory(histolytica_pomp_IZ,params=coef(miff2_Iztapalapa),as=TRUE)

histolytica_pomp_IZ %>% 
  as.data.frame() %>% 
  melt(id="time") %>% 
  ggplot(aes(x=time,y=value))+
  geom_line()+
  geom_path(data=trajectory_histolytica,mapping=aes(x=time,y=S))+
  facet_grid(variable~.,scales="free_y") 

histolytica_pomp_IZ %>% as.data.frame() %>% 
  ggplot(data=trajectory_histolytica)+
  geom_point(mapping=aes(x=Amebiasis,y=cases))+
  facet_grid(variable~.,scales="free_y") 





sims <- simulate(histolytica_pomp_IZ,params=coef(miff2_Iztapalapa),
                 nsim=1,as.data.frame=TRUE,include.data=TRUE)





gg_A<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=cases))+
  geom_line(colour="grey",size=1)+
  geom_line(subset(sims,sim=="data"),mapping=aes(x=time,y=Amebiasis),colour="black")+
  guides(color=FALSE)



gg_C<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=C))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)

gg_S<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=S/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)

gg_E<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=E/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)


gg_I<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=I/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)

gg_R<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=R/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)


grid.arrange(gg_S, gg_E, gg_I,gg_R,gg_C,nrow = 3)
png(filename = "TS_hystolitica_model_outputData.png",width = 18,height = 20,units = "cm",res = 200)
grid.arrange(gg_S, gg_E, gg_I,gg_R,gg_C,nrow = 3)
dev.off()

png(filename = "prediction_vs_data.png",width = 18,height = 10,units = "cm",res = 200)
ggplot_msims
dev.off()



