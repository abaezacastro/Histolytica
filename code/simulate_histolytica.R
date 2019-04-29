#simulate histolytica for Iztapalapa and plot time series
#################################################
sims <- simulate(histolytica_pomp_IZ,params=coef(miff2_Iztapalapa),
                 nsim=1,as.data.frame=TRUE,include.data=TRUE)


plot(sims$cases[85:168],type="l")
plot(sims$S[85:168],type="l")
plot(sims$E[85:168],type="l")
plot(sims$R[85:168],type="l")
plot(sims$C_D[85:168],type="l")
lines(sims$C_E[85:168],col="blue")

N_pop=fixed_params[2]
png(filename = "single_simualtion_example_Iztapalapa.png",width = 12,height = 8,units = "cm",res = 100)
plot.ts(dat_IZ$Amebiasis,ylab="cases Amebiasis")
  lines(sims$cases[86:168],col='red')
legend("topright",legend = c("data","simulation"),col = c("black","red"),lty=c(1,1))
dev.off()

plot(dat_IZ$Amebiasis,sims$cases[85:168],ylim=c(0,300),xlim=c(0,300))
abline(1,1)

png(filename = "predicted_vs_observed.png",width = 12,height = 8,units = "cm",res = 100)
ggplot(sims,mapping=aes(y=Amebiasis,x=cases,color=sim))+
  geom_point(color="black")+
  scale_x_continuous("Observed",limits=c(0,400))+
  scale_y_continuous("Predicted",limits=c(0,400))+
  guides(color=FALSE)+
  geom_abline(intercept = 0)+
  theme(panel.background = element_blank())
dev.off()


################################################################################


histolytica_pomp_IZ %>% 
  simulate(params=coef(miff2_Iztapalapa),nsim=400,as.data.frame=TRUE,include.data=TRUE) %>%
  subset(time>2005,select=c(time,sim,Amebiasis)) %>%
  mutate(data=sim=="data") %>%
  ddply(~time+data,summarize,
        p=c(0.1,0.5,0.9),q=quantile(Amebiasis,prob=p,names=FALSE,na.rm=T)) %>%
  mutate(p=mapvalues(p,from=c(0.1,0.5,0.9),to=c("lo","med","hi")),
         data=mapvalues(data,from=c(TRUE,FALSE),to=c("data","simulation"))) %>%
  dcast(time+data~p,value.var='q') %>%
  ggplot(aes(x=time,y=med,color=data,fill=data,ymin=lo,ymax=hi))+
  geom_ribbon(alpha=I(0.2))+
  theme_bw()+scale_fill_manual(values=c("green","black"), name="fill")->ggplot_msims



sims <- simulate(histolytica_pomp_IZ,params=coef(miff2_Iztapalapa),
                 nsim=1,as.data.frame=TRUE,include.data=TRUE)


sims<-subset(sims,time>2005)


gg_A<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=cases))+
  geom_line(colour="grey",size=1)+
  geom_line(subset(sims,sim=="data"),mapping=aes(x=time,y=Amebiasis),colour="black")+
  guides(color=FALSE)+
  theme_bw()


gg_C<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=cases))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()

gg_S<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=S/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()+ 
  ylab("Suceptible") + 
  xlab("Time")

gg_E<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=E/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()+ 
  ylab("Exposed") + 
  xlab("Time")


gg_I<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=I/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()+ 
  ylab("Infected") + 
  xlab("Time")

gg_CR<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=R/N_pop))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()+ 
  ylab("Immune") + 
  xlab("Time")

##################################
#environmental stages

gg_CE<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=C_E))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()+ 
  ylab("Environmental Risk") + 
  xlab("Time")


gg_CD<-ggplot(subset(sims,sim!="data"),mapping=aes(x=time,y=C_D))+
  geom_line(colour="grey",size=1)+
  guides(color=FALSE)+
  theme_bw()+ 
  ylab("Domiciliary Risk") + 
  xlab("Time")




grid.arrange(ggplot_msims,gg_S, gg_E, gg_I,gg_R,gg_C,nrow = 3)
png(filename = "TS_hystolitica_model_outputData.png",width = 18,height = 20,units = "cm",res = 200)
grid.arrange(ggplot_msims,gg_S, gg_E, gg_I,gg_R,gg_C,nrow = 3)
dev.off()

png(filename = "prediction_vs_data_iztapalapa.png",width = 18,height = 10,units = "cm",res = 200)
ggplot_msims
dev.off()


png(filename = "Unobserved_Epidemiological_states.png",width = 18,height = 10,units = "cm",res = 200)
grid.arrange(gg_S, gg_E, gg_I,gg_R,nrow = 2)

dev.off()



png(filename = "Unobserved_cyst_states.png",width = 18,height = 10,units = "cm",res = 200)
grid.arrange(gg_CD, gg_CE,ncol = 2)

dev.off()

