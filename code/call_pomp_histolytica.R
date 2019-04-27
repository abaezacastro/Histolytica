################################################################################
library(foreach)
#Initial parameter values
fixed_params<-c(d=0.001282,N=1.8E6, delta=0.1,s=0.9,alpha=1,cyst_pp=0.00027,Rain_max=max_rain)#3000000/1.8E 
params <- c(sigPRO=0.01874, #noise
            rho=0.03,#reporting rate
            sigOBS=0.17, #par noise
            beta_D=0.00001,# probability of infection associated to the concentration of cysts in households
            beta_E=0.01,# probability of infection associated to the concentration of cysts in the environtment
            gamma=5, #the effect of rainfall
            sigma=0.5,
            v_r=0.1,
            v0=0.01,
            S_0=0.10,
            E_0=0.05,
            I_0=0.6,
            C_E0=2570, #concentration of pathogens in water. measure as number of cysts in a volume of water 
            C_D0=2570,
            g=0.007,
            w=0.7
)

################################################################################
#particle filter
pf <- pfilter(histolytica_pomp_IZ,Np=1000,params=c(params,fixed_params),verbose=T)
logLik(pf)
plot(pf)


################################################################################
rdd<-rw.sd(sigPRO=0.0001,
           rho=0.0001,
           sigOBS=0.0001,
           beta_D=0.000001,
           beta_E=0.000001,
           gamma=0.001,
           s=0.0001,
           v_r=0.001,
           v0=0.001,
           w=0.00001,
           g=0.00001,
           S_0=ivp(0.0001),
           E_0=ivp(0.0001),
           I_0=ivp(0.0001),
           C_D0=ivp(0.0001),
           C_E0=ivp(0.0001)
)


miff_test<-mif2(histolytica_pomp_IZ,
                Np=1000,
                Nmif=1,
                cooling.type="geometric",
                cooling.fraction.50=0.1,
                transform=TRUE,
                start=coef(pf),
                rw.sd=rdd,
                pred.mean=TRUE,
                filter.mean=TRUE,max.fail=500)

miff2_Iztapalapa<-continue(miff_test)

ll<-numeric(30)
for (i in 1:30){
  miff2_Iztapalapa<-mif2(miff2_Iztapalapa)
  ll[i]<-logLik(miff2_Iztapalapa)
  print(logLik(miff2_Iztapalapa))
  
}
plot.ts(ll)
logLik(miff2_Iztapalapa)
coef(miff2_Iztapalapa)

############################################################################################
#Benckmark 
nb_lik <- function(theta) {
  -sum(dnbinom(as.vector(obs(histolytica_pomp_IZ)),size=exp(theta[1]),prob=exp(theta[2]),log=TRUE))
} 
nb_mle <- optim(c(0,-5),nb_lik)
-nb_mle$value
############################################################################################

#saveRDS(object = miff2_Iztapalapa,file = "histolitica_miff_output_Iztapalapa")
#
#miff2_Iztapalapa<-readRDS("histolitica_miff_output_Iztapalapa")
pf <- pfilter(histolytica_pomp_IZ,Np=10000,params=coef(miff2_Iztapalapa),verbose=T)
logLik(pf)

pf <- pfilter(histolytica_pomp_IZ,Np=10000,params=coef(pf),verbose=T)
logLik(pf)


histolytica_pomp_IZ %>% 
  simulate(params=coef(miff2_Iztapalapa),nsim=400,as.data.frame=TRUE,include.data=TRUE) %>%
  subset(select=c(time,sim,Amebiasis)) %>%
  mutate(data=sim=="data") %>%
  ddply(~time+data,summarize,
        p=c(0.1,0.5,0.9),q=quantile(Amebiasis,prob=p,names=FALSE,na.rm=T)) %>%
  mutate(p=mapvalues(p,from=c(0.1,0.5,0.9),to=c("lo","med","hi")),
         data=mapvalues(data,from=c(TRUE,FALSE),to=c("data","simulation"))) %>%
  dcast(time+data~p,value.var='q') %>%
  ggplot(aes(x=time,y=med,color=data,fill=data,ymin=lo,ymax=hi))+
  geom_ribbon(alpha=I(0.2))+scale_fill_manual(values=c("green","black"), name="fill")->ggplot_msims

